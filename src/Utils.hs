module Utils ( module Utils ) where

escapeChar :: Char -> Maybe Char
escapeChar c = case c of
  'a' -> Just '\a'
  'b' -> Just '\b'
  'f' -> Just '\f'
  'n' -> Just '\n'
  'r' -> Just '\r'
  't' -> Just '\t'
  'v' -> Just '\v'
  _   -> Nothing