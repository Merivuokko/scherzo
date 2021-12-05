{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StrictData #-}

-- | Parser configuration management

module Alma.Parser.Config
    (ParserConfig (..),
     readConfigFile)
where

import Data.Kind (Type)
import Data.HashMap.Strict qualified as Map
import Data.Text qualified as T
import Data.Tuple (swap)
import Dhall (inputFile, auto)

import Alma.Parser.CharTrie as CT

type ParserConfig :: Type
data ParserConfig = ParserConfig {
    tokenTrie :: CT.CharTrie T.Text
    }
    deriving stock (Eq, Read, Show)

type ParserStrings :: Type
type ParserStrings = Map.HashMap T.Text T.Text

mapToCharTrie :: ParserStrings -> CT.CharTrie T.Text
mapToCharTrie = CT.fromList . fmap swap . Map.toList

readConfigFile :: FilePath -> IO ParserConfig
readConfigFile file = do
    val <- inputFile auto file
    pure $! ParserConfig {
        tokenTrie = mapToCharTrie val
        }
