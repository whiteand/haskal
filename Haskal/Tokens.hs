module Haskal.Tokens where

import Data.Char
import Haskal.FileContent
import Haskal.Parser

data Token
  = KeywordProgram
  | Spaces String
  | Id String
  | SemiColon
  | KeywordBegin
  | KeywordEnd
  | Dot
  deriving (Show)

type Error = (SourcePtr, String)

type TokenParser = Parser FileContent Error Token

eofError :: SourcePtr -> Either Error a
eofError pos = Left (pos, "Unexpected EOF")

-- consumeWhile returns a range which has inclusive start
-- and exclusive end. also returns consumed string and
-- remaining parser input
consumeWhile :: (Char -> Bool) -> Parser FileContent a (SourcePtr, SourcePtr, String)
consumeWhile pred = Parser parse
  where
    parse :: ParseFunction FileContent a (SourcePtr, SourcePtr, String)
    parse (Eof pos) = Right ((pos, pos, ""), Eof pos)
    parse (Char pos c rest)
      | pred c = Right ((pos, end, c : restString), remainingFileContent)
      | otherwise = Right ((pos, pos, ""), Char pos c rest)
      where
        Right ((_, end, restString), remainingFileContent) = parse rest

parseId :: TokenParser
parseId = Parser parseId'
  where
    parseId' :: ParseFunction FileContent Error Token
    parseId' (Eof pos) = eofError pos
    parseId' (Char pos firstCharacter rest)
      | isAlpha firstCharacter = Right (Id (firstCharacter : suffixOfParsedId), remainingFileContent)
      | otherwise = Left (pos, "first character is not alpha")
      where
        Right ((_, _, suffixOfParsedId), remainingFileContent) = parse (consumeWhile isAlphaNum) rest

createSingleCharParser :: Char -> (SourcePtr -> Token) -> TokenParser
createSingleCharParser char createToken = Parser {parse = createSingleCharParser'}
  where
    createSingleCharParser' (Eof pos) = eofError pos
    createSingleCharParser' (Char pos c rest)
      | c == char = Right (createToken pos, rest)
      | otherwise = Left (pos, "Expected '" ++ [char] ++ "', but '" ++ [c] ++ "' occurred")

parseIdOrKeyword :: TokenParser
parseIdOrKeyword = fmap convertIdToKeyword parseId
  where
    convertIdToKeyword (Id idString) =
      case map toLower idString of
        "program" -> KeywordProgram
        "begin" -> KeywordBegin
        "end" -> KeywordEnd
        x -> Id idString

parseSpaces :: TokenParser
parseSpaces = Parser parseSpaces'
  where
    parseSpaces' (Eof pos) = eofError pos
    parseSpaces' (Char pos x xs)
      | isSpace x = Right (Spaces (x : restSpaces), remainingParserInput)
      | otherwise = Left (pos, "Not a space")
      where
        Right ((_, _, restSpaces), remainingParserInput) = parse (consumeWhile isSpace) xs

alwaysFail :: String -> Parser FileContent Error a
alwaysFail message = Parser alwaysFail'
  where
    alwaysFail' (Eof pos) = Left (pos, message)
    alwaysFail' (Char pos _ _) = Left (pos, message)

parsers :: [TokenParser]
parsers =
  [ parseIdOrKeyword,
    parseSpaces,
    createSingleCharParser ';' (const SemiColon),
    createSingleCharParser '.' (const Dot),
    alwaysFail "Unexpected token"
  ]

parseTokens :: FileContent -> Either Error [Token]
parseTokens (Eof pos) = Right []
parseTokens content = do
  (token, rest) <- singleTokenResult
  fmap (token :) (parseTokens rest)
  where
    singleTokenResult = foldr1 chooseParseResult [parse p content | p <- parsers]
    chooseParseResult _ (Right r) = Right r
    chooseParseResult nextResult _ = nextResult

stringToTokens :: FilePath -> String -> Either Error [Token]
stringToTokens filePath content = parseTokens (parserInputFromFileContent filePath content)

readFileTokens :: FilePath -> IO [Token]
readFileTokens filePath = do
  content <- readFile filePath
  let tokenizerResult = stringToTokens filePath content
  either failOnError return tokenizerResult
  where
    failOnError (ptr, err) = error (show ptr ++ err)