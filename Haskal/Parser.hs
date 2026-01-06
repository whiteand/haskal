module Haskal.Parser where

import Control.Applicative

type ParseFunction input error a = input -> Either error (a, input)

newtype Parser input error a = Parser
  { parse :: ParseFunction input error a
  }

instance Functor (Parser input error) where
  fmap :: (a -> b) -> Parser input error a -> Parser input error b
  fmap f previousParser = newParser
    where
      newParser = Parser {parse = newParse}
      newParse parseInput = do
        (prevResult, prevRest) <- parse previousParser parseInput
        return (f prevResult, prevRest)

instance Applicative (Parser input error) where
  pure x = Parser (\input -> Right (x, input))
  liftA2 f pa pb =
    Parser
      ( \input -> do
          (a, restA) <- parse pa input
          (b, restB) <- parse pb restA
          return (f a b, restB)
      )

class CannotParse input error where
  createCannotParseError :: input -> error

instance (CannotParse input error) => Alternative (Parser input error) where
  empty = Parser (Left . createCannotParseError)
  pa <|> pb =
    Parser
      ( \input ->
          case parse pa input of
            Right r -> Right r
            Left _ -> parse pb input
      )

instance Monad (Parser input error) where
  (>>=) parserA createParserB =
    Parser
      ( \input -> do
          (a, remaining) <- parse parserA input
          let parserB = createParserB a
          parse parserB remaining
      )

inputParser :: Parser input error input
inputParser = Parser (\input -> Right (input, input))

mapErr :: Parser input error a -> (error -> newError) -> Parser input newError a
mapErr parser createNewError =
  Parser
    ( \input -> do
        case parse parser input of
          Right (r, remaining) -> Right (r, remaining)
          Left e -> Left (createNewError e)
    )

withErr :: e -> Parser input error a -> Parser input e a
withErr error parser = mapErr parser (const error)

class FailWithMessage input error where
  failWithMessage :: String -> input -> error

instance (FailWithMessage input error) => MonadFail (Parser input error) where
  fail message = Parser (Left . failWithMessage message)

parseWhile :: (x -> Bool) -> Parser input error x -> Parser input e [x]
parseWhile pred parser = Parser parseWhile'
  where
    parseWhile' input = case parse parser input of
      Left _ -> Right ([], input)
      Right (x, remaining) ->
        if pred x
          then do
            (xs, remainingInput) <- parseWhile' remaining
            Right (x : xs, remainingInput)
          else
            Right ([], input)