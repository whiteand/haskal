module Main where

import Control.Applicative (asum)
import Data.Char
import Data.Either
import Data.Maybe
import Haskal.FileContent
import Haskal.Parser
import Haskal.Tokens
import System.Environment (getArgs)

formatFile :: FilePath -> IO ()
formatFile filePath = do
  tokens <- readFileTokens filePath
  -- TODO: add writing to the file
  print tokens

formatFiles :: [FilePath] -> IO ()
formatFiles = mapM_ formatFile

main :: IO ()
main = do
  inputFiles <- getArgs
  formatFiles inputFiles
  print inputFiles