{-# LANGUAGE OverloadedStrings #-}

module C.PreProcessor (
    CppOptions(..),
    cpp
    ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as L
import Data.Text.Encoding (decodeUtf8)

import System.Process.Typed
import Control.Concurrent.STM (atomically)

------------------------------------------------------

data CppOptions = CppOptions {
    cppInputFile :: Text,
    cppDefines :: [Text],
    cppIncludes :: [Text]
}

cpp :: CppOptions -> IO Text
cpp opts = withProcess_ cppProcess $ \p -> do
    decodeUtf8 . L.toStrict <$> atomically (getStdout p)
    where
        includes = map (T.append "-I") (cppIncludes opts)
        defines = map (T.append "-D") (cppDefines opts)
        args = includes ++ defines ++ [cppInputFile opts]
        cppProcess =
            setStdout byteStringOutput $
            proc "cpp" $ map T.unpack args

