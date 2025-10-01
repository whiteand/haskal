module Main where

import Control.Applicative (asum)
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

eofError :: SourcePtr -> Either Error a
eofError pos = Left (pos, "Unexpected EOF")

parseId :: TokenParser
parseId [] = error "Unexpected empty position"
parseId ((pos, Nothing) : _) = eofError pos
parseId ((pos, Just firstCharacter) : rest)
  | isAlpha firstCharacter = Right (Id parsedId, restCode)
  | otherwise = Left (pos, "first character is not alpha")
  where
    (parsedIdWithPositions, restCode) = span isAlphaNum' ((pos, Just firstCharacter) : rest)
    parsedId = map (fromJust . snd) parsedIdWithPositions
    isAlphaNum' :: PtrChar -> Bool
    isAlphaNum' (_, Nothing) = False
    isAlphaNum' (_, Just c) = isAlphaNum c

type TokenParser = [PtrChar] -> Either Error (Token, [PtrChar])

createSingleCharParser :: Char -> (SourcePtr -> Token) -> TokenParser
createSingleCharParser char createToken [] = error "Unexpected empty position"
createSingleCharParser char createToken ((pos, Nothing) : _) = eofError pos
createSingleCharParser char createToken ((pos, Just c) : rest)
  | c == char = Right (createToken pos, rest)
  | otherwise = Left (pos, "Expected '" ++ [char] ++ "', but '" ++ [c] ++ "' occurred")

parseIdOrKeyword :: TokenParser
parseIdOrKeyword text =
  fmap convertIdToKeyword (parseId text)
  where
    convertIdToKeyword (Id idString, rest) =
      case map toLower idString of
        "program" -> (KeywordProgram, rest)
        "begin" -> (KeywordBegin, rest)
        "end" -> (KeywordEnd, rest)
        x -> (Id idString, rest)

parseSpaces :: TokenParser
parseSpaces [] = error "Unexpected empty position"
parseSpaces ((pos, Nothing) : _) = Left (pos, "unexpected")
parseSpaces ((pos, Just x) : xs)
  | isSpace x = Right (Spaces spaces, restCode)
  | otherwise = Left (pos, "Not a space")
  where
    (parsedSpacesWithPositions, restCode) = span isSpace' ((pos, Just x) : xs)
    spaces = map (fromJust . snd) parsedSpacesWithPositions
    isSpace' :: PtrChar -> Bool
    isSpace' (_, Nothing) = False
    isSpace' (_, Just c) = isSpace c

alwaysFail :: String -> [PtrChar] -> Either Error a
alwaysFail message [] = error "Unexpected empty position"
alwaysFail message ((pos, _) : _) = Left (pos, message)

parsers :: [TokenParser]
parsers =
  [ parseIdOrKeyword,
    parseSpaces,
    createSingleCharParser ';' (const SemiColon),
    createSingleCharParser '.' (const Dot),
    alwaysFail "Unexpected token"
  ]

parseTokens :: [PtrChar] -> Either Error [Token]
parseTokens [] = error "Unexpected empty position"
parseTokens ((pos, Nothing) : _) = Right []
parseTokens content = do
  (token, rest) <- singleTokenResult
  fmap (token :) (parseTokens rest)
  where
    singleTokenResult = foldr1 chooseParseResult [p content | p <- parsers]
    chooseParseResult _ (Right r) = Right r
    chooseParseResult nextResult _ = nextResult

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

stringToTokens :: FilePath -> String -> Either Error [Token]
stringToTokens filePath content = parseTokens (addPtrsToString filePath content)

readFileTokens :: FilePath -> IO [Token]
readFileTokens filePath = do
  content <- readFile filePath
  let tokenizerResult = stringToTokens filePath content
  either failOnError return tokenizerResult
  where
    failOnError (ptr, err) = error (show ptr ++ err)

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