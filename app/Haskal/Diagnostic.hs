module Haskal.Diagnostic where

import Haskal.FileContent

data DiagnosticLevel = DiagnosticLevelError

instance Show DiagnosticLevel where
  show :: DiagnosticLevel -> String
  show DiagnosticLevelError = "error"

data DiagnosticHeader = DiagnosticHeader
  { level :: DiagnosticLevel,
    content :: String
  }

testHeader :: DiagnosticHeader
testHeader = DiagnosticHeader {level = DiagnosticLevelError, content = "Hello"}

instance Show DiagnosticHeader where
  show :: DiagnosticHeader -> String
  show (DiagnosticHeader {level, content}) = show level ++ ": " ++ content

data DiagnosticWindow
  = DiagnosticWindow
  { sourcePtr :: SourcePtr,
    elements :: [DiagnosticWindowElement]
  }

data DiagnosticWindowElement
  = EmptyLine
  | SourceLine
      { lineNumber :: Int,
        line :: String
      }
  | Label
      { startColumn :: Int,
        endColumn :: Int,
        content :: String
      }

testDiagnosticWindow :: DiagnosticWindow
testDiagnosticWindow = DiagnosticWindow {sourcePtr, elements}
  where
    testSourceLine = SourceLine {lineNumber = 62000, line = "let handle = thread::spawn(|| {"}
    elements = [EmptyLine, testSourceLine, testLabel, EmptyLine]
    sourcePtr = SourcePtr {filePath = "example.pas", lineNumber = 42, column = 32}
    testLabel = Label {startColumn = 28, endColumn = 30, content = "may outlive borrowed value `v`"}

instance Show DiagnosticWindow where
  show :: DiagnosticWindow -> String
  show (DiagnosticWindow {sourcePtr, elements}) =
    unlines (headerString : map renderElement elements)
    where
      lineNumberOffset = replicate maxLineNumberCharsLength ' '
      headerString = lineNumberOffset ++ "--> " ++ show sourcePtr
      getLineNumberCharsLength element = case element of
        EmptyLine -> 0
        SourceLine {lineNumber} -> length $ show lineNumber
        Label {} -> 0
      sourceCodePadding = replicate 5 ' '
      maxLineNumberCharsLength = maximum (map getLineNumberCharsLength elements)
      emptyLinePrefix = lineNumberOffset ++ " |"
      renderElement element = case element of
        EmptyLine -> emptyLinePrefix
        (SourceLine {lineNumber, line}) -> show lineNumber ++ " |" ++ sourceCodePadding ++ line
        (Label {startColumn, endColumn, content}) -> emptyLinePrefix ++ sourceCodePadding ++ offset ++ arrows ++ " " ++ content
          where
            offset = replicate (startColumn - 1) ' '
            arrows = replicate (endColumn - startColumn) '^'

newtype Diagnostic = Diagnostic [DiagnosticElement]

data DiagnosticElement
  = DiagnosticElementHeader DiagnosticHeader
  | DiagnosticElementWindow DiagnosticWindow

testDiagnostic :: Diagnostic
testDiagnostic =
  Diagnostic
    [ DiagnosticElementHeader testHeader,
      DiagnosticElementWindow testDiagnosticWindow
    ]

instance Show Diagnostic where
  show :: Diagnostic -> String
  show (Diagnostic elements) = unlines $ map showElement elements
    where
      showElement (DiagnosticElementHeader header) = show header
      showElement (DiagnosticElementWindow window) = show window