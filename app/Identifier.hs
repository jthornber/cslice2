module Identifier (
    Identifier(..)
    ) where

data Identifier =
    Identifier String
    deriving (Eq, Show, Ord)

