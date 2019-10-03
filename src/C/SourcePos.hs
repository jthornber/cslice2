module C.SourcePos (
    SourcePos(..),
    SourceRange(..)
    ) where

import Data.Text (Text)

data SourcePos = SourcePos {
    sourceLine :: !Int,
    sourceColumn :: !Int,
    sourceFile :: !Text
} deriving (Eq)

instance Show SourcePos where
    show (SourcePos line col file) = show file ++ ":" ++ show line ++ ":" ++ show col

data SourceRange = SourceRange {
    rangeBegin :: !SourcePos,
    rangeEnd :: !SourcePos
} deriving (Show, Eq)
