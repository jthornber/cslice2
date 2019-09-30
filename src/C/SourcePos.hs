module C.SourcePos (
    SourcePos(..),
    SourceRange(..)
    ) where

import Data.Text (Text)

data SourcePos = SourcePos {
    sourceLine :: !Int,
    sourceColumn :: !Int,
    sourceFile :: !Text
} deriving (Show, Eq)

data SourceRange = SourceRange {
    rangeBegin :: !SourcePos,
    rangeEnd :: !SourcePos
} deriving (Show, Eq)
