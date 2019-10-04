module C.ErrorType (
    SliceError(..)
    ) where

import C.SourcePos
import Data.Text (Text)

data SliceError = SliceError {
    errorPos :: Maybe SourcePos,
    errorMessage :: Text
} deriving (Eq, Show)

