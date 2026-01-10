module Haskal.Diagnostic where

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