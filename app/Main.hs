
module Main ( main ) where

import           Codegen                 ( printOut )

import           Control.Lens            ( (^..)
                                         , itraversed
                                         , to
                                         , withIndex )

import           Data.List               ( findIndex
                                         , intercalate )
import           Data.Maybe              ( fromMaybe )
import           Data.String.Interpolate ( i )
import qualified Data.Text.Lazy.IO       as T

import           Parsing

import           System.Environment      ( getArgs )

import           Text.Megaparsec         ( errorBundlePretty
                                         , runParser )
import           Text.Printf             ( printf )

import           Types

main :: IO ()
main = do
  cliArgs <- getArgs
  if length cliArgs /= 1
      then putStrLn "Usage: moonlex <filename>"
      else do
        let filename = head cliArgs
        content <- readFile filename
        case runParser parseSpec filename content of
          Left err   -> putStrLn (errorBundlePretty err)
          Right spec -> printOut spec