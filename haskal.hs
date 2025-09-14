module Main where

import Data.Char
import Data.Either
import System.Environment (getArgs)

data Token
  = KeywordProgram
  | Spaces String
  | Id String
  | SemiColon
  | KeywordBegin
  | KeywordEnd
  | Dot
  deriving (Show)

type SourceFileContent = String

type Error = String

parseId :: SourceFileContent -> Either Error (Token, SourceFileContent)
parseId [] = Left "Unexpect EOF"
parseId (firstCharacter : rest)
  | isAlpha firstCharacter = Right (Id parsedId, restCode)
  | otherwise = Left "first character is not alpha"
  where
    (parsedId, restCode) = span isAlphaNum (firstCharacter : rest)

parseTokens :: SourceFileContent -> Either Error [Token]
parseTokens content = Right [Id "Hello"]

formatFile filePath = do
  content <- readFile filePath
  let tokenizerResult = parseTokens content
  case tokenizerResult of
    Left err -> error err
    Right result -> do
      print result

-- TODO: add writing to the file

formatFiles :: [FilePath] -> IO ()
formatFiles = mapM_ formatFile

main :: IO ()
main = do
  inputFiles <- getArgs
  formatFiles inputFiles
  print inputFiles