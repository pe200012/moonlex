{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Codegen ( module Codegen ) where

import           Automata

import           Control.Lens
                 ( (%~)
                 , (&)
                 , (??)
                 , (^..)
                 , _1
                 , _2
                 , _3
                 , _4
                 , itraversed
                 , to
                 , traversed
                 , withIndex )
import           Control.Monad           ( zipWithM )
import           Control.Monad.State     ( MonadState(..)
                                         , evalState )

import           Data.Bits               ( (.<<.)
                                         , Bits(..) )
import qualified Data.HashMap.Lazy       as HashMap
import qualified Data.HashSet            as HashSet
import           Data.List               ( findIndex
                                         , intercalate
                                         , sortOn )
import           Data.Maybe              ( fromMaybe )
import           Data.String.Interpolate ( i )
import qualified Data.Text.Lazy          as T
import           Data.Text.Lazy          ( Text )
import qualified Data.Text.Lazy.IO       as T
import           Data.Vector             ( (!)
                                         , Vector )
import qualified Data.Vector             as Vector
import           Data.Word               ( Word64 )

import           Text.Printf             ( printf )

import           Types

ruleAny :: Spec -> [(Regex, Action)]
ruleAny spec = [ (r, a) | (conds, r, a) <- rules spec, "*" `elem` conds ]

inclusive :: String -> Spec -> [(Regex, Action)]
inclusive tag spec =
    [ (r, a) | (conds, r, a) <- rules spec, null conds || tag `elem` conds ]
    <> ruleAny spec

exclusive :: String -> Spec -> [(Regex, Action)]
exclusive tag spec =
    [ (r, a) | (conds, r, a) <- rules spec, tag `elem` conds ] <> ruleAny spec

defaults :: Spec -> [(Regex, Action)]
defaults spec = [ (r, a)
                | (conds, r, a) <- rules spec
                , null conds || "INITIAL" `elem` conds
                ] <> ruleAny spec

-- | categorize rules by tag
taggingSpec :: Spec -> [(String, [(Regex, Action)])]
taggingSpec spec =
    (HashMap.toList ((star <>) <$> (((initial <>) <$> incl) <> excl))
     <> ([ ("*", star) | not (null star) ])
     <> [ ("INITIAL", initial) | not (null initial) ])
    & traversed . _2 %~ sortOn (precedence . snd)
  where
    (_, eset) = foldr (\prag (iacc, eacc) -> case directive prag of
                         "s"
                             | not (argument prag `HashSet.member` eacc) ->
                                 (HashSet.insert (argument prag) iacc, eacc)
                         "x"
                             | not (argument prag `HashSet.member` iacc) ->
                                 (iacc, HashSet.insert (argument prag) eacc)
                         _   -> (iacc, eacc))
                      (HashSet.empty, HashSet.empty)
                      (pragmas spec)

    (incl, excl, star, initial) =
        foldr (\(tags, reg, act) acc ->
               if null tags
               then acc
                   & _4 %~ ((reg, act) :)
               else if "*" `elem` tags
                    then acc
                        & _3 %~ ((reg, act) :)
                    else foldr (\tag acc' ->
                                if tag `HashSet.member` eset
                                then acc'
                                    & _2 %~ HashMap.insertWith (<>)
                                                               tag
                                                               [ (reg, act) ]
                                else acc'
                                    & _1 %~ HashMap.insertWith (<>)
                                                               tag
                                                               [ (reg, act) ])
                               acc
                               tags)
              (HashMap.empty, HashMap.empty, [], [])
              (rules spec)

genTransition :: [(Regex, Action)] -> (Int, Vector Action, Vector DFANode)
genTransition rs = (startId, Vector.fromList (snd <$> rs), auto)
  where
    (startId, auto) = minimizeDFA $ evalState ?? 0 $ do
      r <- zipWithM genNFA [ 0 .. ] (fst <$> rs)
      idx <- get
      pure $ genDFA $ (emptyNFANode idx) { epsilonOutN = r }

codegenRule :: Int -> (Int, Vector Action, Vector DFANode) -> (String, String)
codegenRule name (startId, actions, auto) =
    ( if canShift
      then T.unpack [i|let yyaccepted#{name} : Array[Int] = #{yyshiftAccepted}
let yytable#{name} : Array[Int64] = [#{intercalate "," (printf "0x%.XL" <$> yyshiftTable)}]
let yyfailed#{name} : Array[List[Int]] = Array::make(61, List::Nil)
let yybounded#{name} : Array[Bool] = [#{if or yybounded then intercalate ",false,false,false,false,false," (yyboundedStr) else ""}]
|]
      else T.unpack [i|let yyaccepted#{name} : Array[Int] = #{Vector.toList (acceptedByD <$> auto)}
let yytable#{name} : Array[Array[Int]] = #{yytable}
let yyfailed#{name} : Array[List[Int]] = Array::make(#{numStates}, List::Nil)
let yybounded#{name} : Array[Bool] = [#{if or yybounded then intercalate "," yyboundedStr else ""}]
|]
    , T.unpack [i|
              let (yyfinal, yymark, yytext) = yyloop#{fromEnum canShift}(yybuf, yytable#{name}, yyaccepted#{name}, yyfailed#{name}, yybounded#{name}, #{if canShift then fromIntegral startId * bitPerState else fromIntegral startId})
              if yyfinal != (-1) {
                yyoffset.val = yymark
                match yyfinal {
                  (-1) => { yyerror() }
                  #{T.intercalate "\n                " matches}
                  _ => abort("impossible")
                }
              } else {
                yyerror()
              }
    |]
    )
  where
    numStates = Vector.length auto

    canShift = numStates <= 10

    allFinals = Vector.map indexD (Vector.filter isFinal auto)

    isFinal n = acceptedByD n /= -1

    reachableFromFinal n =
        any (\f ->
             walk f
                  auto
                  False
                  (\m -> if indexD n == indexD m then Just True else Nothing))
            allFinals

    yybounded :: [Bool]
    yybounded =
        [ not (isFinal n) && reachableFromFinal n | n <- Vector.toList auto ]

    yyboundedStr = [ if b then "true" else "false" | b <- yybounded ]

    bitPerState :: Word64
    bitPerState = 6

    yyshiftTable :: [Word64]
    yyshiftTable =
        foldr (\x acc -> if x == -1
                         then (acc .<<. 6) .|. 63
                         else (acc .<<. 6) .|. (fromIntegral x * bitPerState))
              0 <$> yytable

    yyshiftAccepted :: [Int]
    yyshiftAccepted = [ if r == 0 && fromIntegral q < numStates
                        then acceptedByD (auto ! fromIntegral q)
                        else -1
                      | idx <- [ 0 .. 60 ]
                      , let (q, r) = quotRem idx bitPerState
                      ]

    yytable :: [[Int]]
    yytable =
        [ Vector.foldr (\node acc ->
                        fromMaybe (-1) (outD node HashMap.!? Unicode alpha)
                        : acc)
                       []
                       auto
        | alpha <- [ minBound .. maxBound ]
        ]

    matches = actions ^.. itraversed . withIndex
        . to (\(idx, act) -> [i|#{idx} => {#{runAction act}
}|])

codegenGlobal :: Text
codegenGlobal =
    [i|
let yyoffset : Ref[Int] = { val : 0 }

fn failed_previously(yyfailedinput : List[Int]) -> Bool {
  match yyfailedinput {
    List::Nil => false
    List::Cons(x, xs) => {
      if x == yyoffset.val {
        true
      } else {
        failed_previously(xs)
      }
    }
  }
}

fn yyloop0(yybuf : Array[Int],
           yytable : Array[Array[Int]],
           yyaccepted : Array[Int],
           yyfailed : Array[List[Int]],
           yybounded : Array[Bool],
           yystart : Int) -> (Int, Int, String) {
  fn go0(yycur : Int,
         yyacc : String,
         yyfinal : List[Int],
         yymark : List[Int],
         yytext : String) -> (List[Int], List[Int], String) {
    if yyoffset.val >= yybuf.length() {
      return (yyfinal, yymark, yytext)
    }
    if yybounded[yycur] && failed_previously(yyfailed[yycur]) {
      return (yyfinal, yymark, yytext)
    }
    let __yy_input__ = yybuf[yyoffset.val]
    let yynext = yytable[__yy_input__][yycur]
    if yynext != (-1) {
      let curoffset = yyoffset.val
      yyoffset.val = yyoffset.val + 1
      let __yy_inputchar__ = Char::from_int(__yy_input__)
      let newyyacc = yyacc + "\\(__yy_inputchar__)"
      let act = yyaccepted[yynext]
      if act != -1 {
        go0(yynext, newyyacc, List::Cons(yynext, List::Nil), List::Cons(yyoffset.val, List::Nil), newyyacc)
      } else {
        if yybounded[yycur] {
          go0(yynext, newyyacc, List::Cons(yycur, yyfinal), List::Cons(curoffset, yymark), yytext)
        } else {
          go0(yynext, newyyacc, yyfinal, yymark, yytext)
        }
      }
    } else {
      return (yyfinal, yymark, yytext)
    }
  }
  fn go1(yycur : Int,
         yyacc : String,
         yyfinal : List[Int],
         yymark : List[Int],
         yytext : String) -> (List[Int], List[Int], String) {
    if yyoffset.val >= yybuf.length() {
      return (yyfinal, yymark, yytext)
    }
    let __yy_input__ = yybuf[yyoffset.val]
    let yynext = yytable[__yy_input__][yycur]
    if yynext != (-1) {
      let curoffset = yyoffset.val
      yyoffset.val = yyoffset.val + 1
      let __yy_inputchar__ = Char::from_int(__yy_input__)
      let newyyacc = yyacc + "\\(__yy_inputchar__)"
      let act = yyaccepted[yynext]
      if act != -1 {
        go1(yynext, newyyacc, List::Cons(yynext, List::Nil), List::Cons(yyoffset.val, List::Nil), newyyacc)
      } else {
        go1(yynext, newyyacc, yyfinal, yymark, yytext)
      }
    } else {
      return (yyfinal, yymark, yytext)
    }
  }
  let (finals, marks, yytext) =
      if yybounded.length() > 0 {
        go0(yystart, "", List::Cons(-1, List::Nil), List::Cons(-1, List::Nil), "")
      } else {
        go1(yystart, "", List::Cons(-1, List::Nil), List::Cons(-1, List::Nil), "")
      }
  fn backtracking(fs : List[Int],
                  ms : List[Int]) -> (Int, Int) {
      match (fs, ms) {
        (List::Nil, List::Nil) => (-1, -1)
        (List::Cons(-1, List::Nil), List::Cons(-1, List::Nil)) => (-1, -1)
        (List::Cons(f, List::Nil), List::Cons(m, List::Nil)) => (yyaccepted[f], m)
        (List::Cons(nf, rfs), List::Cons(nm, rms)) => {
          yyfailed[nf] = List::Cons(nm, yyfailed[nf])
          backtracking(rfs, rms)
        }
        _ => abort("yyloop backtracking")
      }
  }
  let (final, mark) = backtracking(finals, marks)
  (final, mark, yytext)
}
fn yyloop1(yybuf : Array[Int],
           yytable : Array[Int64],
           yyaccepted: Array[Int],
           yyfailed: Array[List[Int]],
           yybounded : Array[Bool],
           yystart : Int) -> (Int, Int, String) {
  fn go0(yycur : Int,
         yyacc : String,
         yyfinal : List[Int],
         yymark : List[Int],
         yytext : String) -> (List[Int], List[Int], String) {
    if yyoffset.val >= yybuf.length() {
      return (yyfinal, yymark, yytext)
    }
    if yybounded[yycur] && failed_previously(yyfailed[yycur]) {
      return (yyfinal, yymark, yytext)
    }
    let __yy_input__ = yybuf[yyoffset.val]
    let yynext = yytable[__yy_input__].lsr(yycur.land(63).to_int64()).land(63L).to_int()
    if yynext != 63 {
      let curoffset = yyoffset.val
      yyoffset.val = yyoffset.val + 1
      let __yy_inputchar__ = Char::from_int(__yy_input__)
      let newyyacc = yyacc + "\\(__yy_inputchar__)"
      let act = yyaccepted[yynext]
      if act != -1 {
        go0(yynext, newyyacc, List::Cons(yynext, List::Nil), List::Cons(yyoffset.val, List::Nil), newyyacc)
      } else {
        if yybounded[yycur] {
          go0(yynext, newyyacc, List::Cons(yycur, yyfinal), List::Cons(curoffset, yymark), yytext)
        } else {
          go0(yynext, newyyacc, yyfinal, yymark, yytext)
        }
      }
    } else {
      return (yyfinal, yymark, yytext)
    }
  }
  fn go1(yycur : Int,
         yyacc : String,
         yyfinal : List[Int],
         yymark : List[Int],
         yytext : String) -> (List[Int], List[Int], String) {
    if yyoffset.val >= yybuf.length() {
      return (yyfinal, yymark, yytext)
    }
    let __yy_input__ = yybuf[yyoffset.val]
    let yynext = yytable[__yy_input__].lsr(yycur.land(63).to_int64()).land(63L).to_int()
    if yynext != 63 {
      let curoffset = yyoffset.val
      yyoffset.val = yyoffset.val + 1
      let __yy_inputchar__ = Char::from_int(__yy_input__)
      let newyyacc = yyacc + "\\(__yy_inputchar__)"
      let act = yyaccepted[yynext]
      if act != -1 {
        go1(yynext, newyyacc, List::Cons(yynext, List::Nil), List::Cons(yyoffset.val, List::Nil), newyyacc)
      } else {
        go1(yynext, newyyacc, yyfinal, yymark, yytext)
      }
    } else {
      return (yyfinal, yymark, yytext)
    }
  }
  let (finals, marks, yytext) =
    if yybounded.length() > 0 {
      go0(yystart, "", List::Cons(63, List::Nil), List::Cons(63, List::Nil), "")
    } else {
      go1(yystart, "", List::Cons(63, List::Nil), List::Cons(63, List::Nil), "")
    }
  fn backtracking(fs : List[Int],
                  ms : List[Int]) -> (Int, Int) {
      match (fs, ms) {
        (List::Nil, List::Nil) => (-1, -1)
        (List::Cons(63, List::Nil), List::Cons(63, List::Nil)) => (-1, -1)
        (List::Cons(f, List::Nil), List::Cons(m, List::Nil)) => (yyaccepted[f], m)
        (List::Cons(nf, rfs), List::Cons(nm, rms)) => {
          yyfailed[nf] = List::Cons(nm, yyfailed[nf])
          backtracking(rfs, rms)
        }
        _ => abort("yyloop backtracking")
      }
  }
  let (final, mark) = backtracking(finals, marks)
  (final, mark, yytext)
}|]

printOut :: Spec -> IO ()
printOut spec = do
  mapM_ putStrLn defs
  T.putStrLn codegenGlobal
  T.putStrLn [i|
enum YYCOND {
  #{intercalate "\n  " (fst <$> allTags)}
}

fn yybegin(cond : YYCOND) {
  match cond {
    #{T.intercalate "\n    " (zipWith genMatchCond (fst <$> allTags) [0..])}
  }
}

let yycond : Ref[Int] = { val : #{initialCond} }

fn yyerror() {
  abort("lexing error")
}

fn yylex() {
  fn yyinit(failed : Array[List[Int]]) {
    var idx = 0
    while(idx < failed.length()) {
      failed[idx] = List::Nil
      idx = idx + 1
    }
  }
  #{intercalate "\n  " initFailed}

  let yybuf : Array[Int] = yyinput()
  yyoffset.val = 0
  while(yyoffset.val < yybuf.length()) {
    match yycond.val {
      #{intercalate "\n      " dispatch}
      _ => abort("impossible")
    }
  }
}|]
  putStrLn (userCode spec)
  where
    allTags = taggingSpec spec

    (defs, loops) = unzip (allTags ^.. itraversed . to snd . to genTransition
                           . withIndex . to (uncurry codegenRule))

    initialCond = fromMaybe (error "cannot find initial condition")
                            (findIndex ((== "INITIAL") . fst) allTags)

    dispatch = loops ^.. itraversed . withIndex
        . to (uncurry (printf "%d => {\n%s\n}"))

    initFailed = [ printf "yyinit(yyfailed%d)" idx
                 | idx <- [ 0 .. length allTags - 1 ]
                 ]

    genMatchCond :: String -> Int -> Text
    genMatchCond t idx = [i|#{t} => { yycond.val = #{idx} }|]
