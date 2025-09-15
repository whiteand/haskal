module Main where

import Data.Char
import Data.Either
import Data.Maybe
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

data SourcePtr
  = SourcePtr
  { filePath :: FilePath,
    lineNumber :: Int,
    column :: Int
  }

instance Show SourcePtr where
  show s = filePath s ++ ":" ++ show (lineNumber s) ++ ":" ++ show (column s)

type Error = (SourcePtr, String)

type PtrChar = (SourcePtr, Maybe Char)

parseId :: [PtrChar] -> Either Error (Token, [PtrChar])
parseId [] = error "Unexpected empty position"
parseId [(pos, Nothing)] = Left (pos, "Unexpected EOF")
parseId ((_, Nothing) : _) = error "Unexpected absence of character"
parseId ((pos, Just firstCharacter) : rest)
  | isAlpha firstCharacter = Right (Id parsedId, restCode)
  | otherwise = Left (pos, "first character is not alpha")
  where
    (parsedIdWithPositions, restCode) = span isAlphaNum' ((pos, Just firstCharacter) : rest)
    parsedId = map (fromJust . snd) parsedIdWithPositions
    isAlphaNum' :: PtrChar -> Bool
    isAlphaNum' (_, Nothing) = False
    isAlphaNum' (_, Just c) = isAlphaNum c

parseTokens :: [PtrChar] -> Either Error [Token]
parseTokens content = Right [Id "Hello"]

nextLineStart :: SourcePtr -> SourcePtr
nextLineStart (SourcePtr filePath lineNumber _) =
  SourcePtr
    { filePath,
      lineNumber = lineNumber + 1,
      column = 1
    }

nextColumn :: SourcePtr -> SourcePtr
nextColumn ptr = ptr {column = column ptr + 1}

addPtrsToString :: FilePath -> String -> [PtrChar]
addPtrsToString filePath = go startPosition
  where
    go :: SourcePtr -> String -> [PtrChar]
    go pos [] = [(pos, Nothing)]
    go pos (x : xs) = (pos, Just x) : restPtrChars
      where
        restPtrChars = go (nextPosition x) xs
        nextPosition '\n' = nextLineStart pos
        nextPosition _ = nextColumn pos

    startPosition = SourcePtr {filePath, lineNumber = 1, column = 1}

formatFile :: FilePath -> IO ()
formatFile filePath = do
  content <- readFile filePath
  let contentWithCoordinates = addPtrsToString filePath content
  let tokenizerResult = parseTokens contentWithCoordinates
  case tokenizerResult of
    Left (ptr, err) -> error (show ptr ++ err)
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