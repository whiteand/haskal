module Haskal.Tokens where

import Control.Applicative
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

instance CannotParse FileContent Error where
  createCannotParseError :: FileContent -> Error
  createCannotParseError input = (getPos input, "Cannot parse")

eofError :: SourcePtr -> Either Error a
eofError pos = Left (pos, "Unexpected EOF")

-- consumeWhile returns a range which has inclusive start
-- and exclusive end. also returns consumed string and
-- remaining parser input
consumeWhile :: (Char -> Bool) -> Parser FileContent a (SourcePtr, SourcePtr, String)
consumeWhile pred = Parser parse
  where
    parse (Eof pos) = Right ((pos, pos, ""), Eof pos)
    parse (Char pos c rest)
      | pred c = Right ((pos, end, c : restString), remainingFileContent)
      | otherwise = Right ((pos, pos, ""), Char pos c rest)
      where
        Right ((_, end, restString), remainingFileContent) = parse rest

parseCharIf :: (Char -> Bool) -> (SourcePtr -> Char -> String) -> Parser FileContent Error (SourcePtr, Char)
parseCharIf predicate getMessage = Parser parseChar
  where
    parseChar :: ParseFunction FileContent Error (SourcePtr, Char)
    parseChar (Eof pos) = eofError pos
    parseChar (Char pos char rest)
      | predicate char = Right ((pos, char), rest)
      | otherwise = Left (pos, getMessage pos char)

parseAlpha :: Parser FileContent Error (SourcePtr, Char)
parseAlpha = parseCharIf isAlpha (const (const "Expected alpha character"))

parseManyAlphaNum :: Parser FileContent Error (SourcePtr, SourcePtr, String)
parseManyAlphaNum = Parser (parse (consumeWhile isAlphaNum))

parseId :: TokenParser
parseId = liftA2 combineFirstAndRest parseAlpha parseManyAlphaNum
  where
    combineFirstAndRest (_, c) (_, _, rest) = Id (c : rest)

createSingleCharParser :: Char -> (SourcePtr -> Token) -> TokenParser
createSingleCharParser char createToken =
  fmap
    (createToken . fst)
    ( parseCharIf
        (== char)
        (\pos c -> "Expected '" ++ [char] ++ "', but '" ++ [c] ++ "' occurred")
    )

parseIdOrKeyword :: TokenParser
parseIdOrKeyword = fmap convertIdToKeyword parseId
  where
    convertIdToKeyword (Id idString) =
      case map toLower idString of
        "program" -> KeywordProgram
        "begin" -> KeywordBegin
        "end" -> KeywordEnd
        x -> Id idString

parseSpaces :: Parser FileContent Error String
parseSpaces =
  Parser
    ( \text -> do
        ((pos, _, restSpaces), remainingFileContent) <- parse (consumeWhile isSpace) text
        case restSpaces of
          [] -> Left (pos, "Not a space")
          spaces -> Right (spaces, remainingFileContent)
    )

parseSpacesToken = fmap Spaces parseSpaces

alwaysFail :: String -> Parser FileContent Error a
alwaysFail message = Parser alwaysFail'
  where
    alwaysFail' (Eof pos) = Left (pos, message)
    alwaysFail' (Char pos _ _) = Left (pos, message)

tokenParser :: TokenParser
tokenParser =
  parseIdOrKeyword
    <|> parseSpacesToken
    <|> createSingleCharParser ';' (const SemiColon)
    <|> createSingleCharParser '.' (const Dot)
    <|> alwaysFail "Unexpected token"

parseTokens :: FileContent -> Either Error [Token]
parseTokens (Eof pos) = Right []
parseTokens content = do
  (token, rest) <- parse tokenParser content
  fmap (token :) (parseTokens rest)

stringToTokens :: FilePath -> String -> Either Error [Token]
stringToTokens filePath content = parseTokens (parserInputFromFileContent filePath content)

readFileTokens :: FilePath -> IO [Token]
readFileTokens filePath = do
  content <- readFile filePath
  let tokenizerResult = stringToTokens filePath content
  either failOnError return tokenizerResult
  where
    failOnError (ptr, err) = error (show ptr ++ err)

tp :: Parser FileContent e a -> String -> Either e (a, FileContent)
tp parser input = parse parser (parserInputFromFileContent "t.pas" input)