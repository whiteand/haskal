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

data ParserInput = Eof SourcePtr | Char SourcePtr Char ParserInput
  deriving (Show)

instance Show SourcePtr where
  show s = filePath s ++ ":" ++ show (lineNumber s) ++ ":" ++ show (column s)

type Error = (SourcePtr, String)

type PtrChar = (SourcePtr, Maybe Char)

eofError :: SourcePtr -> Either Error a
eofError pos = Left (pos, "Unexpected EOF")

-- consumeWhile returns a range which has inclusive start
-- and exclusive end. also returns consumed string and
-- remaining parser input
consumeWhile :: (Char -> Bool) -> ParserInput -> (SourcePtr, SourcePtr, String, ParserInput)
consumeWhile _ (Eof pos) = (pos, pos, "", Eof pos)
consumeWhile pred (Char pos c rest)
  | pred c = (pos, end, c : restString, remainingParserInput)
  | otherwise = (pos, pos, "", Char pos c rest)
  where
    (_, end, restString, remainingParserInput) = consumeWhile pred rest

parseId :: TokenParser
parseId (Eof pos) = eofError pos
parseId (Char pos firstCharacter rest)
  | isAlpha firstCharacter = Right (Id (firstCharacter : suffixOfParsedId), remainingParserInput)
  | otherwise = Left (pos, "first character is not alpha")
  where
    (_, _, suffixOfParsedId, remainingParserInput) = consumeWhile isAlphaNum rest

type TokenParser = ParserInput -> Either Error (Token, ParserInput)

createSingleCharParser :: Char -> (SourcePtr -> Token) -> TokenParser
createSingleCharParser char createToken (Eof pos) = eofError pos
createSingleCharParser char createToken (Char pos c rest)
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
parseSpaces (Eof pos) = eofError pos
parseSpaces (Char pos x xs)
  | isSpace x = Right (Spaces (x : restSpaces), remainingParserInput)
  | otherwise = Left (pos, "Not a space")
  where
    (_, _, restSpaces, remainingParserInput) = consumeWhile isSpace xs

alwaysFail :: String -> ParserInput -> Either Error a
alwaysFail message (Eof pos) = Left (pos, message)
alwaysFail message (Char pos _ _) = Left (pos, message)

parsers :: [TokenParser]
parsers =
  [ parseIdOrKeyword,
    parseSpaces,
    createSingleCharParser ';' (const SemiColon),
    createSingleCharParser '.' (const Dot),
    alwaysFail "Unexpected token"
  ]

parseTokens :: ParserInput -> Either Error [Token]
parseTokens (Eof pos) = Right []
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

parserInputFromFileContent :: FilePath -> String -> ParserInput
parserInputFromFileContent filePath = go startPosition
  where
    go :: SourcePtr -> String -> ParserInput
    go pos [] = Eof pos
    go pos (x : xs) = Char pos x restParserInput
      where
        restParserInput = go (nextPosition x) xs
        nextPosition '\n' = nextLineStart pos
        nextPosition _ = nextColumn pos

    startPosition = SourcePtr {filePath, lineNumber = 1, column = 1}

stringToTokens :: FilePath -> String -> Either Error [Token]
stringToTokens filePath content = parseTokens (parserInputFromFileContent filePath content)

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