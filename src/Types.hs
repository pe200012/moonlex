{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Types ( module Types ) where

import           Data.Functor.Foldable    ( cata )
import           Data.Functor.Foldable.TH ( makeBaseFunctor )
import           Data.List                ( intercalate
                                          , nub )
import           Data.Word                ( Word8 )

data Pragma = MkPragma { directive :: String, argument :: String }
    deriving ( Show
             , Eq
             , Ord )

data Repeat = Exactly Int | FromTo Int (Maybe Int)
    deriving ( Show
             , Eq )

data Regex
    = Char Word8
    | Concatenation [Regex]
    | Choice [Regex]
    | WordSet [Word8]
    | Complement [Word8]
    | ZeroOrMore Regex
    | Option Regex
    | Any
    | Group Regex
    | EndOfString
    | EmptySet
    | Epsilon
    deriving ( Show
             , Eq )

makeBaseFunctor ''Regex

ppRegex :: Regex -> String
ppRegex = cata go
  where
    go :: RegexF String -> String
    go (CharF w) = [ toEnum (fromIntegral w) ]
    go (ConcatenationF xs) = concat xs
    go (ChoiceF xs) = "(" <> intercalate "|" xs <> ")"
    go (WordSetF xs) = "[" <> (toEnum . fromIntegral <$> nub xs) <> "]"
    go (ComplementF xs) = "[^" <> (toEnum . fromIntegral <$> nub xs) <> "]"
    go (ZeroOrMoreF x)
        | length x > 1 = "(" <> x <> ")*"
        | otherwise = x <> "*"
    go (OptionF x)
        | length x > 1 = "(" <> x <> ")?"
        | otherwise = x <> "?"
    go AnyF = "."
    go (GroupF x) = "(" <> x <> ")"
    go EndOfStringF = "$"
    go EmptySetF = "∅"
    go EpsilonF = "ε"

concatenationR :: [Regex] -> Regex
concatenationR [] = Epsilon
concatenationR [ x ] = x
concatenationR xs = case foldr phi [] xs of
  []    -> Epsilon
  [ x ] -> x
  _     -> Concatenation xs
  where
    phi Epsilon acc = acc
    phi EmptySet _ = [ EmptySet ]
    phi _ [ EmptySet ] = [ EmptySet ]
    phi r acc = r : acc

choiceR :: [Regex] -> Regex
choiceR [] = EmptySet
choiceR [ x ] = x
choiceR xs = case foldr phi [] xs of
  []    -> EmptySet
  [ x ] -> x
  _     -> Choice xs
  where
    phi Epsilon _ = [ Epsilon ]
    phi EmptySet acc = acc
    phi _ [ Epsilon ] = [ Epsilon ]
    phi r acc = r : acc

wordSetR :: [Word8] -> Regex
wordSetR [] = EmptySet
wordSetR [ x ] = Char x
wordSetR xs = WordSet (nub xs)

complementR :: [Word8] -> Regex
complementR [] = Any
complementR xs = Complement (nub xs)

repeatR :: Repeat -> Regex -> Regex
repeatR (Exactly 0) _ = Epsilon
repeatR (Exactly 1) r = r
repeatR (Exactly n) r = Concatenation (replicate n r)
repeatR (FromTo 0 Nothing) r = ZeroOrMore r
repeatR (FromTo 1 Nothing) r = Concatenation [ r, ZeroOrMore r ]
repeatR (FromTo n Nothing) r = Concatenation (replicate n r <> [ ZeroOrMore r ])
repeatR (FromTo 0 (Just 0)) _ = Epsilon
repeatR (FromTo 0 (Just 1)) r = Option r
repeatR (FromTo 0 (Just n)) r = Concatenation (replicate n r)
repeatR (FromTo n (Just m)) r =
    Concatenation (replicate n r
                   <> [ Option (Concatenation (replicate (m - n) r)) ])

zeroOrMoreR :: Regex -> Regex
zeroOrMoreR EmptySet = Epsilon
zeroOrMoreR (Option r) = ZeroOrMore r
zeroOrMoreR r = ZeroOrMore r

oneOrMoreR :: Regex -> Regex
oneOrMoreR EmptySet = EmptySet
oneOrMoreR (Option r) = ZeroOrMore r
oneOrMoreR r = Concatenation [ r, ZeroOrMore r ]

optionR :: Regex -> Regex
optionR EmptySet = Epsilon
optionR Epsilon = Epsilon
optionR (Option r) = Option r
optionR r = Option r

data Action = Action { precedence :: Int, runAction :: String }
    deriving ( Show
             , Eq )

data Spec = Spec { rules    :: [([String], Regex, Action)]
                 , userCode :: String
                 , pragmas  :: [Pragma]
                 }
    deriving ( Show )
