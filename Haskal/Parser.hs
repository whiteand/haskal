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
