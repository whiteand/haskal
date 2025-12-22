module Haskal.Tokens where

import Control.Applicative
import Data.Char
import Haskal.FileContent
import Haskal.Helper
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

type Error = (SourcePtr, String)

type TokenParser = Parser FileContent Error Token

instance CannotParse FileContent Error where
  createCannotParseError :: FileContent -> Error
  createCannotParseError input = (getPos input, "Cannot parse")

instance FailWithMessage FileContent Error where
  failWithMessage message input = (getPos input, message)

posParser :: Parser FileContent e SourcePtr
posParser = getPos <$> inputParser

isEofParser :: Parser FileContent e Bool
isEofParser = Parser parseIsEof
  where
    parseIsEof (Eof pos) = Right (True, Eof pos)
    parseIsEof input = Right (False, input)

anyCharParser :: Parser FileContent Error Char
anyCharParser = Parser parseAnyChar
  where
    parseAnyChar (Eof pos) = eofError pos
    parseAnyChar (Char _ char remaining) = Right (char, remaining)

parseCharIf :: (Char -> Bool) -> String -> Parser FileContent Error Char
parseCharIf pred expectedCharacterClass = do
  c <- anyCharParser
  if pred c
    then return c
    else fail ("Expected " ++ expectedCharacterClass ++ ", but '" ++ [c] ++ "' occurred")

eofError :: SourcePtr -> Either Error a
eofError pos = Left (pos, "Unexpected EOF")

parseIdOrKeyword :: TokenParser
parseIdOrKeyword = fmap convertIdToKeyword parseId
  where
    convertIdToKeyword (Id idString) =
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
        x -> Id idString

consumeWhile :: (Char -> Bool) -> Parser FileContent a String
consumeWhile pred = do
  isEof <- isEofParser
  if isEof
    then return []
    else do
      c <- withErr undefined anyCharParser
      if pred c
        then (c :) <$> consumeWhile pred
        else return []

parseId :: TokenParser
parseId = do
  alpha <- parseCharIf isAlpha "alpha character"
  remaining <- consumeWhile isAlphaNum
  return (Id (alpha : remaining))

parseExactChar :: Char -> Parser FileContent Error Char
parseExactChar char = parseCharIf (== char) ("'" ++ [char] ++ "'")

createSingleCharParser :: Char -> Token -> TokenParser
createSingleCharParser char token = token <$ parseExactChar char

parseSpaces :: Parser FileContent Error String
parseSpaces = do
  s <- parseCharIf isSpace "whitespace"
  restSpaces <- consumeWhile isSpace
  return (s : restSpaces)

parseSpacesToken = fmap Spaces parseSpaces

alwaysFail :: String -> Parser FileContent Error a
alwaysFail message = Parser alwaysFail'
  where
    alwaysFail' (Eof pos) = Left (pos, message)
    alwaysFail' (Char pos c _) = Left (pos, message)

exactParser :: [(String, r)] -> Parser FileContent Error r
exactParser m = foldr ((<|>) . createParser) (alwaysFail (expectedOneOf m)) m
  where
    createParser :: (String, r) -> Parser FileContent Error r
    createParser (message, r) = r <$ prefixParser message
      where
        prefixParser :: String -> Parser FileContent Error String
        prefixParser [] = return []
        prefixParser (c : remaining) = do
          _ <- parseExactChar c
          _ <- prefixParser remaining
          return (c : remaining)

    expectedOneOf :: [(String, r)] -> String
    expectedOneOf m = "Expected one of: " ++ expectedList
      where
        expectedStrings = map fst m
        expectedWithQuotes = map (\s -> "\"" ++ s ++ "\"") expectedStrings
        expectedList = listJoin "|" expectedWithQuotes

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

parseTokens :: FileContent -> Either Error [Token]
parseTokens (Eof pos) = Right []
parseTokens content = do
  (token, rest) <- parse tokenParser content
  fmap (token :) (parseTokens rest)

tokensParser :: Parser FileContent Error [Token]
tokensParser = do
  isEof <- isEofParser
  if isEof
    then return []
    else do
      token <- tokenParser
      tokens <- tokensParser
      return (token : tokens)

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

printFileTokens :: FilePath -> IO ()
printFileTokens filePath = do
  text <- readFile filePath
  let fileContent = parserInputFromFileContent filePath text
  let tokensResults = getTokenResults fileContent
  mapM_ printResult tokensResults
  where
    getTokenResults input = case parse tokenParser input of
      Right (token, rest) -> Right token : getTokenResults rest
      Left e -> [Left e]
    printResult (Left e) = print e
    printResult (Right r) = print r

-- >>> readFileTokens "example.pas"
-- example.pas:8:15Unexpected token

directiveParser :: TokenParser
directiveParser = Directive <$> directiveStringParser
  where
    directiveStringParser :: Parser FileContent Error String
    directiveStringParser = liftA3 combine prefixParser parseUntilCloseCurly parseCloseCurly
    combine :: String -> String -> Char -> String
    combine prefix text close = prefix ++ text ++ [close]
    prefixParser :: Parser FileContent Error String
    prefixParser = liftA2 (\c d -> [c, d]) parseOpenCurly parseDollar
    parseOpenCurly :: Parser FileContent Error Char
    parseOpenCurly = parseExactChar '{'
    parseDollar :: Parser FileContent Error Char
    parseDollar = parseExactChar '$'
    parseUntilCloseCurly :: Parser FileContent Error String
    parseUntilCloseCurly = consumeWhile (/= '}')
    parseCloseCurly :: Parser FileContent Error Char
    parseCloseCurly = parseExactChar '}'
