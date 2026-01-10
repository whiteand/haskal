module Haskal.Tokens where

import Control.Applicative
import Data.Char
import Data.List (intercalate)
import Haskal.FileContent
import Haskal.Parser

data Token
  = KeywordProgram
  | Spaces String
  | Id String
  | SemiColon
  | KeywordBegin
  | KeywordUses
  | TypeBoolean
  | TypeInteger
  | TypeString
  | TypeLongint
  | KeywordArray
  | OperatorEqual
  | OpenParens
  | CloseParens
  | OpenBrackets
  | CloseBrackets
  | KeywordRecord
  | Colon
  | Comma
  | KeywordType
  | Directive String
  | KeywordEnd
  | Dot
  deriving (Show)

newtype ParsingError = ParsingError (SourcePtr, String)

type TokenParser = Parser FileContent ParsingError Token

instance CannotParse FileContent ParsingError where
  createCannotParseError :: FileContent -> ParsingError
  createCannotParseError input = ParsingError (getPos input, "Cannot parse")

instance FailWithMessage FileContent ParsingError where
  failWithMessage message input = ParsingError (getPos input, message)

posParser :: Parser FileContent e SourcePtr
posParser = getPos <$> inputParser

isEofParser :: Parser FileContent e Bool
isEofParser = Parser parseIsEof
  where
    parseIsEof (Eof pos) = Right (True, Eof pos)
    parseIsEof input = Right (False, input)

anyCharParser :: Parser FileContent ParsingError Char
anyCharParser = Parser parseAnyChar
  where
    parseAnyChar (Eof pos) = eofError pos
    parseAnyChar (Char _ char remaining) = Right (char, remaining)

parseCharIf :: (Char -> Bool) -> String -> Parser FileContent ParsingError Char
parseCharIf predicate expectedCharacterClass = do
  c <- anyCharParser
  if predicate c
    then return c
    else fail ("Expected " ++ expectedCharacterClass ++ ", but '" ++ [c] ++ "' occurred")

eofError :: SourcePtr -> Either ParsingError a
eofError pos = Left $ ParsingError (pos, "Unexpected EOF")

parseIdOrKeyword :: TokenParser
parseIdOrKeyword = convertIdToKeyword <$> parseIdString
  where
    convertIdToKeyword idString =
      case map toLower idString of
        "program" -> KeywordProgram
        "begin" -> KeywordBegin
        "end" -> KeywordEnd
        "uses" -> KeywordUses
        "type" -> KeywordType
        "boolean" -> TypeBoolean
        "longint" -> TypeLongint
        "array" -> KeywordArray
        "integer" -> TypeInteger
        "record" -> KeywordRecord
        "string" -> TypeString
        _ -> Id idString

consumeWhile :: (Char -> Bool) -> Parser FileContent a String
consumeWhile predicate = parseWhile predicate anyCharParser

parseIdString :: Parser FileContent ParsingError String
parseIdString = do
  alpha <- parseCharIf isAlpha "alpha character"
  remaining <- consumeWhile isAlphaNum
  return (alpha : remaining)

parseExactChar :: Char -> Parser FileContent ParsingError Char
parseExactChar char = parseCharIf (== char) ("'" ++ [char] ++ "'")

createSingleCharParser :: Char -> Token -> TokenParser
createSingleCharParser char token = token <$ parseExactChar char

parseSpaces :: Parser FileContent ParsingError String
parseSpaces = do
  s <- parseCharIf isSpace "whitespace"
  restSpaces <- consumeWhile isSpace
  return (s : restSpaces)

parseSpacesToken :: Parser FileContent ParsingError Token
parseSpacesToken = fmap Spaces parseSpaces

alwaysFail :: String -> Parser FileContent ParsingError a
alwaysFail message = Parser (Left . ParsingError . (,message) . getPos)

exactParser :: [(String, r)] -> Parser FileContent ParsingError r
exactParser m = foldr ((<|>) . createParser) (alwaysFail (expectedOneOf m)) m
  where
    createParser :: (String, r) -> Parser FileContent ParsingError r
    createParser (message, r) = r <$ prefixParser message
      where
        prefixParser :: String -> Parser FileContent ParsingError String
        prefixParser [] = return []
        prefixParser (c : remaining) = do
          _ <- parseExactChar c
          _ <- prefixParser remaining
          return (c : remaining)

    expectedOneOf :: [(String, r)] -> String
    expectedOneOf pairs = "Expected one of: " ++ expectedList
      where
        expectedStrings = map fst pairs
        expectedWithQuotes = map (\s -> "\"" ++ s ++ "\"") expectedStrings
        expectedList = intercalate "|" expectedWithQuotes

tokenParser :: TokenParser
tokenParser =
  parseIdOrKeyword
    <|> parseSpacesToken
    <|> directiveParser
    <|> exactParser
      [ (";", SemiColon),
        (".", Dot),
        ("=", OperatorEqual),
        ("(", OpenParens),
        (":", Colon),
        (")", CloseParens),
        (",", Comma),
        ("[", OpenBrackets),
        ("]", CloseBrackets)
      ]
    <|> alwaysFail "Unexpected token"

stringToTokensResults :: FilePath -> String -> [Either ParsingError Token]
stringToTokensResults filePath content = parseTokens (parserInputFromFileContent filePath content)
  where
    parseTokens :: FileContent -> [Either ParsingError Token]
    parseTokens (Eof _) = []
    parseTokens remaining = do
      case parse tokenParser remaining of
        Left e -> [Left e]
        Right (token, rest) -> Right token : parseTokens rest

tp :: Parser FileContent e a -> String -> Either e (a, FileContent)
tp parser input = parse parser (parserInputFromFileContent "t.pas" input)

directiveParser :: TokenParser
directiveParser =
  Directive <$> do
    _ <- parseExactChar '{'
    _ <- parseExactChar '$'
    directive <- consumeWhile (/= '}')
    _ <- parseExactChar '}'
    return ("{$" ++ directive ++ "}")
