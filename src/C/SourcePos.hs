module C.SourcePos (
    SourcePos(..),
    SourceRange(..),
    Pos(..)
    ) where

import Data.Text (Text)

data SourcePos = SourcePos {
    sourceIndex :: !Int,
    sourceLine :: !Int,
    sourceColumn :: !Int,
    sourceFile :: !Text
} deriving (Eq)

instance Show SourcePos where
    show (SourcePos _ line col file) = show file ++ ":" ++ show line ++ ":" ++ show col

class Pos a where
    getPos :: a -> SourcePos

data SourceRange = SourceRange {
    rangeBegin :: !SourcePos,
    rangeEnd :: !SourcePos
} deriving (Show, Eq)
