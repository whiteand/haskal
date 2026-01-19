module Main where

import Control.Monad (forM_)
import Haskal.Tokens
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)

formatFile :: FilePath -> IO ()
formatFile filePath = do
  content <- readFile filePath
  let tokensResults = stringToTokensResults filePath content
  forM_ tokensResults handleTokenResult
  where
    handleTokenResult (Left diagnostic) = do
      print diagnostic
      exitFailure
    handleTokenResult (Right token) =
      print token

formatFiles :: [FilePath] -> IO ()
formatFiles = mapM_ formatFile

main :: IO ()
main = do
  inputFiles <- getArgs
  formatFiles inputFiles
  print inputFiles
  exitSuccess