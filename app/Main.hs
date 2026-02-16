module Main where

import Control.Monad
import Haskal.Diagnostic (Diagnostic)
import Haskal.Tokens
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)

formatTokens :: [Either Diagnostic Token] -> [Either Diagnostic Token]
formatTokens [] = []
formatTokens (Right OperatorAssign : Right (Spaces s) : rest) =
  map Right [OperatorAssign, Spaces s] ++ formatTokens rest
formatTokens (Right OperatorAssign : Right x : rest) =
  map Right [OperatorAssign, Spaces " ", x] ++ formatTokens rest
formatTokens (Right (Id identifier) : Right OperatorAssign : rest) =
  (Right <$> [Id identifier, Spaces " ", OperatorAssign]) ++ formatTokens rest
formatTokens (Right KeywordProcedure : Right (Spaces _) : Right (Id identifier) : rest) =
  (Right <$> [KeywordProcedure, Spaces " ", Id identifier]) ++ formatTokens rest
formatTokens (Left x : _) = [Left x]
formatTokens (Right x : xs) = Right x : formatTokens xs

formatFile :: FilePath -> IO ()
formatFile filePath = do
  content <- readFile filePath
  let tokensResults = stringToTokensResults filePath content
  let transformedTokens = formatTokens tokensResults
  tokenStrings <- concat <$> forM transformedTokens handleTokenResult
  writeFile filePath tokenStrings
  where
    handleTokenResult (Left diagnostic) = do
      print diagnostic
      exitFailure
    handleTokenResult (Right token) =
      return (renderToken token)

formatFiles :: [FilePath] -> IO ()
formatFiles = mapM_ formatFile

main :: IO ()
main = do
  inputFiles <- getArgs
  formatFiles inputFiles
  print inputFiles
  exitSuccess