{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StrictData #-}

-- | Parser configuration management

module Alma.Syntax.Config
    (SyntaxConfig (..),
     readConfigFile)
where

import Data.Kind (Type)
import Data.HashMap.Strict qualified as Map
import Data.Text qualified as T
import Data.Tuple (swap)
import Dhall (inputFile, auto)

import Alma.Parser.CharTrie as CT

type SyntaxConfig :: Type
data SyntaxConfig = SyntaxConfig {
    tokenTrie :: CT.CharTrie T.Text
    }
    deriving stock (Eq, Read, Show)

type SyntaxStrings :: Type
type SyntaxStrings = Map.HashMap T.Text T.Text

mapToCharTrie :: SyntaxStrings -> CT.CharTrie T.Text
mapToCharTrie = CT.fromList . fmap swap . Map.toList

readConfigFile :: FilePath -> IO SyntaxConfig
readConfigFile file = do
    val <- inputFile auto file
    pure $! SyntaxConfig {
        tokenTrie = mapToCharTrie val
        }
