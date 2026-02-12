module Haskal.Tokens where

import Control.Applicative
import Data.Char
import Data.List (intercalate)
import Haskal.Diagnostic
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
  | MultilineComment String
  | SingleLineComment String
  | OperatorNotEqual
  | OperatorMinus
  | OperatorPlus
  | OperatorStar
  | OperatorLessThan
  | OperatorLessThanOrEqual
  | OperatorGreaterThan
  | OperatorGreaterThanOrEqual
  | OpenParens
  | CloseParens
  | OpenBrackets
  | StringLiteral String
  | CloseBrackets
  | IntegerLiteral String
  | KeywordRecord
  | Colon
  | Comma
  | KeywordType
  | Directive String
  | KeywordEnd
  | DoubleDot
  | Dot
  deriving (Show)

newtype ParsingError = ParsingError (SourcePtr, String)
  deriving (Show)

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
  let isAlphaOrDash x = isAlpha x || x == '_'
  alpha <- parseCharIf isAlphaOrDash "alpha character"
  let isAlphaNumOrDash x = isAlphaNum x || x == '_'
  remaining <- consumeWhile isAlphaNumOrDash
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
    <|> MultilineComment <$> multiLineCommentStringParser
    <|> SingleLineComment <$> singleLineCommentStringParser
    <|> IntegerLiteral <$> integerLiteralStringParser
    <|> StringLiteral <$> stringLiteralStringParser
    <|> exactParser
      [ (";", SemiColon),
        (":", Colon),
        ("..", DoubleDot),
        (".", Dot),
        (",", Comma),
        ("(", OpenParens),
        (")", CloseParens),
        ("[", OpenBrackets),
        ("]", CloseBrackets),
        ("-", OperatorMinus),
        ("+", OperatorMinus),
        ("=", OperatorEqual),
        ("*", OperatorStar),
        ("<>", OperatorNotEqual),
        ("<=", OperatorLessThanOrEqual),
        ("<", OperatorLessThan),
        (">=", OperatorGreaterThanOrEqual),
        (">", OperatorGreaterThan)
      ]
    <|> alwaysFail "Unexpected token"

stringToTokensResults :: FilePath -> String -> [Either Diagnostic Token]
stringToTokensResults filePath sourceCode = mapErrorToDiagnostic <$> parseTokens fileContent
  where
    mapErrorToDiagnostic (Right r) = Right r
    mapErrorToDiagnostic (Left err) = Left (errorToDiagnostic fileContent err)

    fileContent = filePathAndSourceCodeToFileContent filePath sourceCode
    parseTokens :: FileContent -> [Either ParsingError Token]
    parseTokens (Eof _) = []
    parseTokens remaining = do
      case parse tokenParser remaining of
        Left e -> [Left e]
        Right (token, rest) -> Right token : parseTokens rest

errorToDiagnostic :: FileContent -> ParsingError -> Diagnostic
errorToDiagnostic fileContent (ParsingError (ptr, message)) = Diagnostic elements
  where
    elements =
      [ DiagnosticElementHeader header,
        DiagnosticElementWindow sourceCodeWindow
      ]
    header = DiagnosticHeader {level = DiagnosticLevelError, content = message}
    sourceCodeWindow =
      DiagnosticWindow
        { sourcePtr = ptr,
          elements = windowElements
        }
    windowElements =
      [ EmptyLine,
        sourceLine,
        label,
        EmptyLine
      ]
    SourcePtr {lineNumber, column} = ptr
    sourceLine = SourceLine {lineNumber, line = getLineText lineNumber fileContent}
    label = Label {startColumn = column, endColumn = column + 1, content = message}

tp :: Parser FileContent e a -> String -> Either e (a, FileContent)
tp parser input = parse parser (filePathAndSourceCodeToFileContent "t.pas" input)

-- >>> tp tokenParser "1"
-- Left (ParsingError (t.pas:1:1,"Unexpected token"))

directiveParser :: TokenParser
directiveParser =
  Directive <$> do
    _ <- parseExactChar '{'
    _ <- parseExactChar '$'
    directive <- consumeWhile (/= '}')
    _ <- parseExactChar '}'
    return ("{$" ++ directive ++ "}")

decimalDigitParser :: Parser FileContent ParsingError Char
decimalDigitParser = parseCharIf isDigit "digit"

integerLiteralStringParser :: Parser FileContent ParsingError String
integerLiteralStringParser = do
  digit <- decimalDigitParser
  if digit == '0'
    then do
      input <- inputParser
      let result = parse decimalDigitParser input
      case result of
        Left _ -> return "0"
        Right _ -> fail "IntegerLiteral cannot start with 0 when it is not zero"
    else do
      restDigits <- consumeWhile isDigit
      return (digit : restDigits)

stringLiteralStringParser :: Parser FileContent ParsingError String
stringLiteralStringParser = do
  _ <- parseExactChar '\''
  content <- consumeWhile (/= '\'')
  _ <- parseExactChar '\''
  return content

singleLineCommentStringParser :: Parser FileContent ParsingError String
singleLineCommentStringParser = do
  _ <- parseExactChar '/'
  _ <- parseExactChar '/'
  consumeWhile (/= '\n')

multiLineCommentStringParser :: Parser FileContent ParsingError String
multiLineCommentStringParser = do
  _ <- parseExactChar '{'
  content <- consumeWhile (/= '}')
  _ <- parseExactChar '}'
  return content