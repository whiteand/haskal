in Parser.hs

```haskell
inputParser :: Parser input error input
mapErr :: Parser input error a -> (error -> newError) -> Parser input newError a
withErr :: e -> Parser input error a -> Parser input e a

class FailWithMessage input error where
  failWithMessage :: String -> input -> error

instance (FailWithMessage input error) => MonadFail (Parser input error) where
  fail = undefined
```

in Tokens.hs

```haskell
posParser :: Parser FileContent e SourcePtr
isEofParser :: Parser FileContent e Bool
anyCharParser :: Parser FileContent Error Char
parseCharIf :: (Char -> Bool) -> String -> Parser FileContent Error Char
parseExactChar :: Char -> Parser FileContent Error Char
consumeWhile :: (Char -> Bool) -> Parser FileContent a String
parseId :: TokenParser
parseSpaces :: Parser FileContent Error String

-- where prefixParser :: String -> Parser FileContent Error String

tokensParser :: Parser FileContent Error [Token]
```

- write nonNegativeIntegerDecimalStringParser
- add `IntegerLiteral String` to list of tokens
- write `integerLiteralTokenParser`