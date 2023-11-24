{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Parsing ( module Parsing ) where

import           Control.Applicative  ( Applicative(liftA2) )
import           Control.Monad        ( guard
                                      , void )

import qualified Data.ByteString      as B
import           Data.Char
                 ( GeneralCategory(DecimalNumber, LowercaseLetter, UppercaseLetter)
                 , ord )
import           Data.Foldable        ( fold )
import           Data.Functor         ( ($>) )
import           Data.List            ( intercalate )
import           Data.Maybe           ( fromMaybe )
import qualified Data.Text            as T
import           Data.Text.Encoding   ( encodeUtf8 )
import           Data.Void            ( Void )
import           Data.Word            ( Word8 )

import           Text.Megaparsec
import           Text.Megaparsec.Char

import           Types

import           Utils                ( escapeChar )

type MetaGrammarM = Parsec Void String

lexeme :: MetaGrammarM a -> MetaGrammarM a
lexeme p = p <* space

metaChar :: MetaGrammarM Char
metaChar = do
  _ <- char '\\'
  c <- anySingle
  pure $ fromMaybe c (escapeChar c)

singleChar :: MetaGrammarM Char
singleChar = metaChar <|> anySingle

comment_ :: MetaGrammarM ()
comment_ = void comment

comment :: MetaGrammarM String
comment = do
  s <- string "//"
  (s <>) <$> manyTill anySingle (void eol <|> eof)

plainChar :: MetaGrammarM Char
plainChar = charCategory UppercaseLetter <|> charCategory LowercaseLetter
    <|> charCategory DecimalNumber

percentPercent :: MetaGrammarM ()
percentPercent = void $ string "%%"

parsePragma :: MetaGrammarM Pragma
parsePragma = do
  _ <- char '%'
  notFollowedBy (char '%')
  dct <- lexeme $ some plainChar
  arg <- manyTill anySingle eol
  pure $ MkPragma dct arg

positiveSet :: MetaGrammarM Regex
positiveSet = do
  _ <- char '['
  wordSetR <$> parseSetItems

negativeSet :: MetaGrammarM Regex
negativeSet = do
  _ <- string "[^"
  complementR <$> parseSetItems

regexSet :: MetaGrammarM Regex
regexSet = negativeSet <|> positiveSet

groupRegex :: [(String, Regex)] -> MetaGrammarM Regex
groupRegex = between (char '(') (char ')') . parseRegex

dotAny :: MetaGrammarM Regex
dotAny = char '.' $> Any

eoi :: MetaGrammarM Regex
eoi = char '$' $> EndOfString

parseString :: MetaGrammarM String
parseString = do
  _ <- char '"'
  str <- manyTill (Left <$> (string "\\\\\\\"" $> "\\\"") <|> Left
                   <$> (string "\\\"" $> "\"") <|> Right <$> anySingle)
                  (char '"')
  pure (concatMap (either id pure) str)

parseStartCondition :: MetaGrammarM [String]
parseStartCondition =
    between (char '<')
            (char '>')
            (sepBy1 (some (charCategory UppercaseLetter)) (char ','))

quoted :: MetaGrammarM Regex
quoted =
    concatenationR . fmap Char . B.unpack . encodeUtf8 . T.pack <$> parseString

parseRepeat :: MetaGrammarM Repeat
parseRepeat = do
  _ <- lexeme (char '{')
  a <- lexeme $ manyTill digitChar (oneOf @[] ",}")
  option (Exactly (read a)) $ do
    _ <- lexeme (char ',')
    option (FromTo (read a) Nothing) $ FromTo (read a) . Just . read <$> phi
  where
    phi :: MetaGrammarM String
    phi = (space >> char '}' $> "") <|> liftA2 (:) digitChar phi

parseSetItem :: MetaGrammarM [Word8]
parseSetItem = do
  c0 <- singleChar
  option (pure (fromIntegral (ord c0))) $ do
    _ <- char '-'
    c1 <- singleChar
    pure [ fromIntegral (ord c0) .. fromIntegral (ord c1) ]

parseSetItems :: MetaGrammarM [Word8]
parseSetItems = char ']' $> [] <|> liftA2 (<>) parseSetItem parseSetItems

elementaryRegex :: [(String, Regex)] -> MetaGrammarM Regex
elementaryRegex ctx = notFollowedBy (string ")")
    >> (regexSet <|> groupRegex ctx <|> dotAny <|> eoi <|> quoted
        <|> (do
               name <- try namedPattern
               case lookup name ctx of
                 Nothing -> error $ "Unknown pattern: " <> name
                 Just r  -> pure r)
        <|> try (do
                   c <- singleChar
                   guard (c /= '|' && c /= '\n' && c /= '\r' && c /= ' ')
                   case B.unpack (encodeUtf8 (T.singleton c)) of
                     [ w ] -> pure (Char w)
                     xs    -> pure (concatenationR (fmap Char xs))))

basicRegex :: [(String, Regex)] -> MetaGrammarM Regex
basicRegex ctx = do
  r <- elementaryRegex ctx
  choice [ char '*' $> zeroOrMoreR r
         , char '+' $> oneOrMoreR r
         , char '?' $> optionR r
         , liftA2 repeatR (try parseRepeat) (pure r)
         , pure r
         ]

concatenation :: [(String, Regex)] -> MetaGrammarM Regex
concatenation = fmap concatenationR . some . basicRegex

parseRegex :: [(String, Regex)] -> MetaGrammarM Regex
parseRegex ctx = do
  reg <- sepBy1 (concatenation ctx) (char '|')
  pure $ choiceR reg

parseDefinition :: MetaGrammarM (String, Regex)
parseDefinition = do
  name <- lexeme $ some plainChar
  space
  def <- parseRegex []
  pure (name, def)

parseDefinitions :: MetaGrammarM ([(String, Regex)], [Pragma])
parseDefinitions = phi [] []
  where
    phi defs prag = (lexeme comment_ >> phi defs prag)
        <|> (try (lexeme parsePragma) >>= \p -> phi defs (p : prag))
        <|> (try (lexeme parseDefinition) >>= \d -> phi (d : defs) prag)
        <|> pure (reverse defs, reverse prag)

namedPattern :: MetaGrammarM String
namedPattern = do
  _ <- char '{'
  name <- some plainChar
  _ <- char '}'
  pure name

parseCodeblock :: MetaGrammarM String
parseCodeblock = intercalate "\n "
    <$> many (some spaceChar *> manyTill anySingle eol <* many eol)

parseAction :: Int -> MetaGrammarM Action
parseAction p = Action p <$> parseCodeblock

parseRules :: [(String, Regex)] -> MetaGrammarM [([String], Regex, Action)]
parseRules defs = go 0 []
  where
    go i acc = skipMany (lexeme (string "//" >> manyTill anySingle eol))
        >> ((do
               notFollowedBy (string "%%")
               conds <- fold <$> optional parseStartCondition
               pat <- parseRegex defs
               action <- parseAction i
               space
               go (i + 1) ((conds, pat, action) : acc)) <|> pure (reverse acc))

parseSpec :: MetaGrammarM Spec
parseSpec = do
  space
  (def, prag) <- lexeme parseDefinitions
  percentPercent
  space
  rs <- lexeme (parseRules def)
  percentPercent
  space
  uc <- takeRest
  pure $ Spec { rules = rs, userCode = uc, pragmas = prag }