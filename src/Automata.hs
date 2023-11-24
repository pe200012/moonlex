{-# LANGUAGE DeriveGeneric #-}

{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE RankNTypes #-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module Automata ( module Automata ) where

import           Control.Applicative      ( Applicative(liftA2) )
import           Control.Lens
                 ( (<<+=)
                 , (^..)
                 , (^?!)
                 , _1
                 , _2
                 , _3
                 , at
                 , folded
                 , ix
                 , lens
                 , to )
import           Control.Monad            ( foldM
                                          , unless )
import           Control.Monad.ST.Strict  ( runST )
import           Control.Monad.State
                 ( MonadState(..)
                 , evalState
                 , gets
                 , modify
                 , runState )

import           Data.Foldable            ( for_ )
import           Data.Functor.Foldable    ( cata )
import           Data.Functor.Foldable.TH ( makeBaseFunctor )
import           Data.HashMap.Lazy        ( HashMap )
import qualified Data.HashMap.Lazy        as HashMap
import qualified Data.HashSet             as HashSet
import           Data.Hashable            ( Hashable )
import           Data.List                ( foldl'
                                          , partition )
import           Data.Maybe               ( catMaybes
                                          , fromMaybe )
import           Data.Set                 ( Set )
import qualified Data.Set                 as Set
import           Data.Vector              ( (!)
                                          , (//)
                                          , Vector
                                          , unsafeFreeze )
import qualified Data.Vector              as Vector
import           Data.Vector.Mutable      ( unsafeNew
                                          , unsafeRead
                                          , unsafeWrite )
import qualified Data.Vector.Mutable      as MV
import           Data.Word                ( Word8 )

import           Development.Placeholders ( notImplemented )

import           GHC.Generics             ( Generic )

import           Types

-- | NFA
-- with int as state. Starts from state 0
data NFA = NFA { transition :: Vector (HashMap Alphabet (Set Int))
               , epsilonTransition :: Vector [Int]
               , final :: Vector Bool
               }
    deriving ( Show )

emptyNFA :: NFA
emptyNFA =
    NFA { transition = mempty, epsilonTransition = mempty, final = mempty }

newtype Alphabet = Unicode { unAlphabet :: Word8 }
    deriving ( Show
             , Eq
             , Generic
             , Ord )

instance Hashable Alphabet

data NFANode = NFANode { index       :: Int
                       , outN        :: HashMap Alphabet [NFANode]
                       , epsilonOutN :: [NFANode]
                       , acceptedByN :: Maybe Int
                       , loopOutN    :: Maybe Int
                       }
    deriving ( Show
             , Ord
             , Eq )

makeBaseFunctor ''NFANode

emptyNFANode :: Int -> NFANode
emptyNFANode idx = NFANode idx mempty mempty Nothing Nothing

accept :: Int -> NFANode -> NFANode
accept i n = n { acceptedByN = Just i }

moveBy :: NFANode -> [Alphabet] -> NFANode -> NFANode
moveBy a [] b = a { epsilonOutN = b : epsilonOutN a }
moveBy a as b =
    a { outN = foldl' (\m a -> HashMap.insertWith (<>) a [ b ] m) (outN a) as }

moveE :: NFANode -> NFANode -> NFANode
moveE = flip moveBy []

moveToE :: NFANode -> NFANode -> NFANode
moveToE = flip moveE

moveTo :: NFANode -> [Alphabet] -> NFANode -> NFANode
moveTo b as a = moveBy a as b

loopMove :: Int -> NFANode -> NFANode
loopMove i a = a { loopOutN = Just i }

genNFA :: (MonadState Int m) => Int -> Regex -> m NFANode
genNFA tag reg = cata go reg . flip moveE . accept tag . emptyNFANode
    =<< (nodeCounter <<+= 1)
  where
    nodeCounter = lens id (const id)

    go :: (MonadState Int m)
       => RegexF ((NFANode -> NFANode) -> m NFANode)
       -> (NFANode -> NFANode)
       -> m NFANode
    go (CharF w) k = do
      idx <- nodeCounter <<+= 1
      idxEnd <- nodeCounter <<+= 1
      pure (moveBy (emptyNFANode idx) [ Unicode w ] (k (emptyNFANode idxEnd)))
    go (ConcatenationF xs) k = phi xs
      where
        phi [] = k . emptyNFANode <$> (nodeCounter <<+= 1)
        phi [ x ] = x k
        phi (x : rest) = x . moveToE =<< phi rest
    go (ChoiceF xs) k = liftA2 (foldl' moveE . emptyNFANode)
                               (nodeCounter <<+= 1)
                               (mapM ($ k) xs)
    go (WordSetF ws) k = do
      idx <- nodeCounter <<+= 1
      idxEnd <- nodeCounter <<+= 1
      pure (moveBy (emptyNFANode idx)
                   (Unicode <$> ws)
                   (k (emptyNFANode idxEnd)))
    go (ComplementF ws) k = do
      idx <- nodeCounter <<+= 1
      idxEnd <- nodeCounter <<+= 1
      pure (moveBy (emptyNFANode idx)
                   (Unicode <$> filter (`notElem` ws) [ minBound .. maxBound ])
                   (k (emptyNFANode idxEnd)))
    go (ZeroOrMoreF r) k = do
      idx <- nodeCounter <<+= 1
      k . moveE (emptyNFANode idx) <$> r (loopMove idx)
    go (OptionF r) k = do
      idx <- nodeCounter <<+= 1
      idxEnd <- nodeCounter <<+= 1
      let endNode = k (emptyNFANode idxEnd)
      moveE (emptyNFANode idx `moveE` endNode) <$> r (`moveE` endNode)
    go AnyF k = do
      idx <- nodeCounter <<+= 1
      idxEnd <- nodeCounter <<+= 1
      pure (moveBy (emptyNFANode idx)
                   (Unicode <$> [ minBound .. maxBound ])
                   (k (emptyNFANode idxEnd)))
    go (GroupF r) k = r k
    go EndOfStringF _ = $notImplemented
    go EmptySetF _ = emptyNFANode <$> (nodeCounter <<+= 1)
    go EpsilonF k = k <$> (emptyNFANode <$> (nodeCounter <<+= 1))

data DFANode =
    DFANode { indexD :: Int, outD :: HashMap Alphabet Int, acceptedByD :: Int }
    deriving ( Show )

emptyDFANode :: Int -> DFANode
emptyDFANode idx = DFANode idx mempty (-1)

genDFA :: NFANode -> (Int, Vector DFANode)
genDFA n = (startId, vec)
  where
    allAlphabet = Unicode <$> [ minBound .. maxBound ]

    maxIndex = cata go n
      where
        go (NFANodeF idx out eOut _ _) =
            max idx (max (foldl' (foldl' max) 0 out) (foldl' max 0 eOut))

    allNodes = runST $ do
      v <- unsafeNew (maxIndex + 1)
      visited <- MV.replicate (maxIndex + 1) False
      let go node = do
            yes <- unsafeRead visited (index node)
            unless yes $ do
              unsafeWrite visited (index node) True
              unsafeWrite v
                          (index node)
                          ( fmap index <$> outN node
                          , let es = index <$> epsilonOutN node
                            in
                                maybe es (: es) (loopOutN node)
                          , acceptedByN node
                          )
              mapM_ (mapM_ go) (outN node)
              mapM_ go (epsilonOutN node)
      go n
      unsafeFreeze v

    eClosure :: Set Int -> Int -> Set Int
    eClosure visited node =
        foldl' (\vis target ->
                if Set.member target vis then vis else eClosure vis target)
               (if Set.member node visited
                then visited
                else Set.insert node visited)
               (allNodes ^?! ix node . _2)

    eClosure' :: Int -> Set Int
    eClosure' = eClosure mempty

    step :: Alphabet -> Set Int -> Set Int
    step alpha =
        foldl' (\res x ->
                foldl' (flip Set.insert)
                       res
                       (fromMaybe [] (allNodes ^?! ix x . _1 . at alpha)))
               mempty

    closure :: Alphabet -> Int -> Set Int
    closure alpha node = foldl' eClosure mempty (step alpha (eClosure' node))

    walk :: (MonadState (HashMap (Set Int) DFANode) m) => Set Int -> m ()
    walk ns = gets (HashMap.!? ns) >>= \case
      Nothing -> do
        si <- gets HashMap.size
        modify (HashMap.insert ns (emptyDFANode si))
        vs <- foldM (\us alpha ->
                     let es = Set.unions (closure alpha <$> Set.toList ns)
                     in
                         do
                           unless (null es) $ walk es
                           gets (maybe us
                                       (\node ->
                                        HashMap.insert alpha (indexD node) us)
                                 . (HashMap.!? es)))
                    HashMap.empty
                    allAlphabet
        modify (HashMap.insert ns
                               (DFANode { indexD      = si
                                        , outD        = vs
                                        , acceptedByD = case catMaybes (ns ^.. folded . to (\idx -> allNodes ^?! ix idx . _3)) of
                                            [] -> -1
                                            xs -> minimum xs
                                        }))
      _       -> pure ()

    start = eClosure' (index n)

    (startId, maps) = runState (do
                                  walk start
                                  gets (indexD . (HashMap.! start)))
                               mempty

    vec = runST $ do
      v <- unsafeNew (HashMap.size maps)
      for_ (HashMap.elems maps) $ \v' -> unsafeWrite v (indexD v') v'
      unsafeFreeze v

walk :: Int -> Vector DFANode -> a -> (DFANode -> Maybe a) -> a
walk startId vec default_ f =
    fromMaybe default_ (evalState (go startId) HashSet.empty)
  where
    early [] = pure Nothing
    early (x : xs) = go x >>= \case
      Nothing -> early xs
      x'      -> pure x'

    go idx = get >>= \visited ->
        if HashSet.member idx visited
        then pure Nothing
        else case f (vec ! idx) of
          Just x  -> pure (Just x)
          Nothing -> do
            modify (HashSet.insert idx)
            early (HashMap.elems (outD (vec ! idx)))

allTrans :: Int -> Vector DFANode -> [Int]
allTrans i vec = [ HashMap.findWithDefault (-1) a out
                 | a <- Unicode <$> [ minBound .. maxBound ]
                 ]
  where
    out = outD (vec ! i)

nequiv :: Vector DFANode -> Int -> Int -> Int -> Bool
nequiv vec step i j = evalState (go (step, i, j)) HashMap.empty
  where
    go (_, -1, -1) = pure True
    go (_, -1, _) = pure False
    go (_, _, -1) = pure False
    go (0, i, j) = pure (acceptedByD (vec ! i) == acceptedByD (vec ! j))
    go (s, i0, j0) = do
      sub <- go (s - 1, i0, j0)
      if sub
          then do
            m <- get
            case HashMap.lookup (s, i0, j0) m of
              Just b  -> pure b
              Nothing -> do
                b <- and <$> sequence [ go (s - 1, a, b)
                                      | a <- allTrans i0 vec
                                      | b <- allTrans j0 vec
                                      ]
                modify (HashMap.insert (s, i0, j0) b)
                pure b
          else pure False

equivDFASets :: Vector DFANode -> [[Int]]
equivDFASets vec = head (until (\case
                                  (x : y : _) -> x == y
                                  _           -> error "impossible")
                               tail
                               (quoti <$> [ 0 .. ]))
  where
    quoti n = quotientSet (nequiv vec n) [ 0 .. Vector.length vec - 1 ]

quotientSet :: (a -> a -> Bool) -> [a] -> [[a]]
quotientSet eq = go []
  where
    go acc [] = acc
    go acc (x : xs) =
        let (same, diff) = partition (eq x) xs in go ((x : same) : acc) diff

rewrite :: Vector DFANode -> Vector Int -> Vector DFANode
rewrite vec mp = Vector.map (\n -> n { outD = fmap (mp !) (outD n) }) vec

delete :: Int -> Vector DFANode -> (DFANode -> Bool) -> (Int, Vector DFANode)
delete startId vec toBeRemoved =
    ( mp ! startId
    , Vector.zipWith (\n i -> n { indexD = i })
                     (rewrite vec' mp)
                     (Vector.generate (Vector.length vec') id)
    )
  where
    (vec', mp, _) =
        Vector.foldl' (\(acc, mp0, ctr) n ->
                       if toBeRemoved n
                       then (acc, mp0 // [ (indexD n, -1) ], ctr + 1)
                       else ( Vector.snoc acc n
                            , mp0 // [ (indexD n, indexD n - ctr) ]
                            , ctr
                            ))
                      (Vector.empty, Vector.generate (Vector.length vec) id, 0)
                      vec

minimizeDFA :: (Int, Vector DFANode) -> (Int, Vector DFANode)
minimizeDFA (startId, vec) = (s3, stage1)
  where
    reachable n =
        walk (indexD n)
             vec
             False
             (\n' -> if acceptedByD n' == -1 then Nothing else Just True)
        && walk startId
                vec
                False
                (\n' -> if indexD n' == indexD n then Just True else Nothing)

    -- filter out all unreachable
    (s1, stage0) = delete startId vec (not . reachable)

    (mp0, deleted) =
        foldr (\xs (m, d) -> case xs of
                 []         -> (m, d)
                 [ _ ]      -> (m, d)
                 (x : rest) ->
                     (m // ((, x) <$> rest), d <> Vector.fromList rest))
              (Vector.generate (Vector.length stage0) id, Vector.empty)
              (equivDFASets stage0)

    s2 = mp0 ! s1

    (s3, stage1) =
        delete s2 (rewrite stage0 mp0) ((`Vector.elem` deleted) . indexD)