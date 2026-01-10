module Haskal.FileContent where

data SourcePtr
  = SourcePtr
  { filePath :: FilePath,
    lineNumber :: Int,
    column :: Int
  }

data FileContent = Eof SourcePtr | Char SourcePtr Char FileContent
  deriving (Show)

getPos :: FileContent -> SourcePtr
getPos (Eof pos) = pos
getPos (Char pos _ _) = pos

instance Show SourcePtr where
  show s = filePath ++ ":" ++ show lineNumber ++ ":" ++ show column
    where
      SourcePtr {filePath, lineNumber, column} = s

type PtrChar = (SourcePtr, Maybe Char)

nextLineStart :: SourcePtr -> SourcePtr
nextLineStart (SourcePtr filePath lineNumber _) =
  SourcePtr
    { filePath,
      lineNumber = lineNumber + 1,
      column = 1
    }

nextColumn :: SourcePtr -> SourcePtr
nextColumn ptr = ptr {column = prevColumn + 1}
  where
    SourcePtr {column = prevColumn} = ptr

parserInputFromFileContent :: FilePath -> String -> FileContent
parserInputFromFileContent filePath = go startPosition
  where
    go :: SourcePtr -> String -> FileContent
    go pos [] = Eof pos
    go pos (x : xs) = Char pos x restParserInput
      where
        restParserInput = go (nextPosition x) xs
        nextPosition '\n' = nextLineStart pos
        nextPosition _ = nextColumn pos

    startPosition = SourcePtr {filePath, lineNumber = 1, column = 1}