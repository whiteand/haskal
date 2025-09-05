module Main where

import Data.Char
import Data.List
import Data.Maybe

data Token
  = KeywordProgram
  | KeywordBegin
  | KeywordEnd
  | KeywordWriteLine
  | KeywordWrite
  | Identifier String
  | Spaces String
  | StringLiteral String
  | SemiColon
  | OpenParens
  | CloseParens
  | Dot
  deriving (Show)

renderToken :: Token -> String
renderToken KeywordBegin = "begin"
renderToken KeywordEnd = "end"
renderToken Dot = "."
renderToken KeywordWrite = "Write"
renderToken KeywordWriteLine = "WriteLn"
renderToken KeywordProgram = "program"
renderToken OpenParens = "("
renderToken CloseParens = ")"
renderToken SemiColon = ";"
renderToken (Identifier name) = name
renderToken (Spaces spaces) = spaces
renderToken (StringLiteral value) = value

pascalCode :: String
pascalCode =
  "\
  \program HelloWorld;\n\
  \begin\n\
  \ writeln('Hello, World')\n\
  \end.\n\
  \"

pascalTokens :: [Token]
pascalTokens =
  [ KeywordProgram,
    Spaces " ",
    Identifier "HelloWorld",
    SemiColon,
    Spaces "\n",
    KeywordBegin,
    Spaces "\n ",
    KeywordWriteLine,
    OpenParens,
    StringLiteral "Hello, World",
    CloseParens,
    Spaces "\n",
    KeywordEnd,
    Dot,
    Spaces "\n"
  ]

startsWithSpaceOrEof :: String -> Bool
startsWithSpaceOrEof [] = True
startsWithSpaceOrEof (n : _) = isSpace n

type SourceCode = String

type IdentifierString = String

type RestSourceCode = String

parseIdentifer :: SourceCode -> Maybe (IdentifierString, RestSourceCode)
parseIdentifer [] = Nothing
parseIdentifer (n : ns)
  | isAlpha n = Just (identifierString, restSourceCode)
  | otherwise = Nothing
  where
    identifierString = n : takeWhile isAlphaNum ns
    restSourceCode = dropWhile isAlphaNum ns

type KeywordString = String

sameIdentifier :: String -> String -> Bool
sameIdentifier id1 id2 = map toLower id1 == map toLower id2

parseKeyword :: KeywordString -> SourceCode -> Maybe RestSourceCode
parseKeyword keyword source = do
  (identifier, rest) <- parseIdentifer source
  if sameIdentifier keyword identifier
    then return rest
    else Nothing

idToKeyword :: IdentifierString -> Token
idToKeyword id
  | sameIdentifier "program" id = KeywordProgram
  | sameIdentifier "begin" id = KeywordBegin
  | sameIdentifier "end" id = KeywordEnd
  | sameIdentifier "writeln" id = KeywordWriteLine
  | sameIdentifier "write" id = KeywordWrite
  | otherwise = Identifier id

parseStringLiteral :: SourceCode -> Maybe (Token, String)
parseStringLiteral [] = Nothing
parseStringLiteral ('\'' : rest) = Just (StringLiteral stringValue, restString)
  where
    -- TODO: Use something than init
    stringValue = takeWhile (not . (== '\'')) rest
    -- TODO: Use something than tail
    restString = tail (dropWhile (not . (== '\'')) rest)
parseStringLiteral _ = Nothing

parseTokens :: String -> [Token]
parseTokens [] = []
parseTokens ('.' : rest) = Dot : parseTokens rest
parseTokens (';' : rest) = SemiColon : parseTokens rest
parseTokens ('(' : rest) = OpenParens : parseTokens rest
parseTokens (')' : rest) = CloseParens : parseTokens rest
parseTokens str
  | Just (identifier, rest) <- parseIdentifer str =
      idToKeyword identifier : parseTokens rest
  | isSpace (head str) = Spaces (takeWhile isSpace str) : parseTokens (dropWhile isSpace str)
  | Just (token, rest) <- parseStringLiteral str = token : parseTokens rest
  | otherwise = error ("Failed to parse token at: '" ++ str ++ "'")

main :: IO ()
main = do
  putStrLn "Hello, what is your name?"
  name <- getLine
  putStrLn ("Hello, " ++ name ++ "!")
