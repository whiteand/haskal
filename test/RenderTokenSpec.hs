module Main (main) where

import Control.Monad (forM_)
import Haskal.Tokens (Token (..), renderToken)
import Test.Hspec

main :: IO ()
main = hspec $
  describe "renderToken" $
    it "matches expected text for every token shape" $
      forM_ renderTokenCases $ \(token, expected) ->
        renderToken token `shouldBe` expected

renderTokenCases :: [(Token, String)]
renderTokenCases =
  [ -- Payload tokens
    (Directive "{$I+}", "{$I+}"),
    (Id "foo_bar", "foo_bar"),
    (IntegerLiteral "42", "42"),
    (MultilineComment " note ", "{ note }"),
    (SingleLineComment " line", "// line"),
    (Spaces "  \t", "  \t"),
    (StringLiteral "a", "'a'"),
    (StringLiteral "", "''"),
    -- Punctuation and delimiters
    (CloseBrackets, "]"),
    (CloseParens, ")"),
    (Colon, ":"),
    (Comma, ","),
    (Dot, "."),
    (DoubleDot, ".."),
    (OpenBrackets, "["),
    (OpenParens, "("),
    (SemiColon, ";"),
    -- Keywords
    (KeywordArray, "array"),
    (KeywordBegin, "begin"),
    (KeywordConst, "const"),
    (KeywordDo, "do"),
    (KeywordElse, "else"),
    (KeywordEnd, "end"),
    (KeywordFor, "for"),
    (KeywordFunction, "function"),
    (KeywordIf, "if"),
    (KeywordNot, "not"),
    (KeywordOf, "of"),
    (KeywordOperator, "operator"),
    (KeywordProcedure, "procedure"),
    (KeywordProgram, "program"),
    (KeywordRecord, "record"),
    (KeywordThen, "then"),
    (KeywordType, "type"),
    (KeywordUses, "uses"),
    (KeywordVar, "var"),
    -- Operators
    (OperatorAssign, ":="),
    (OperatorEqual, "="),
    (OperatorGreaterThan, ">"),
    (OperatorGreaterThanOrEqual, ">="),
    (OperatorLessThan, "<"),
    (OperatorLessThanOrEqual, "<="),
    (OperatorMinus, "-"),
    (OperatorNotEqual, "<>"),
    (OperatorPlus, "+"),
    (OperatorStar, "*"),
    -- Type names
    (TypeBoolean, "boolean"),
    (TypeInteger, "integer"),
    (TypeLongint, "longint"),
    (TypeString, "string")
  ]
