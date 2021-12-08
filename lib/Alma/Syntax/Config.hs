{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StrictData #-}

-- | Syntax configuration

module Alma.Syntax.Config
    (SyntaxConfig (..),
     TokenMap (map, trie),
     readConfigFile,
     SStateName,
     SState,
     Command)
where

import Data.Kind (Type)
import Data.HashMap.Strict qualified as HM
import Data.Text qualified as T
import Dhall

import Alma.Parser.CharTrie as CT

type SyntaxConfig :: Type
data SyntaxConfig = SyntaxConfig {
    states :: HM.HashMap SStateName SState,
    initState :: SStateName
    }
    deriving stock (Eq, Generic, Show)
    deriving anyclass (FromDhall)

type TokenMap :: Type -> Type
data TokenMap v = TokenMap {
    map :: HM.HashMap T.Text v,
    trie :: CT.CharTrie v
    }
    deriving stock (Eq, Show)

instance (FromDhall v) => FromDhall (TokenMap v) where
    autoWith _ = mkTokenMap <$> hashMap strictText auto

mkTokenMap :: forall v. HM.HashMap T.Text v -> TokenMap v
mkTokenMap m = TokenMap m (CT.fromHashMap m)

readConfigFile :: FilePath -> IO SyntaxConfig
readConfigFile file = inputFile auto file

type SStateName :: Type
type SStateName = T.Text

type SState :: Type
data SState = SState {
    init :: Command,
    fail :: Command,
    tokens :: TokenMap Command
    }
    deriving stock (Eq, Generic, Show)
    deriving anyclass (FromDhall)
                      
type Command :: Type
data Command
    = EnterState SStateName
    | LeaveState
    | Finish
    | Fail
    | Write T.Text
    deriving stock (Eq, Generic, Ord, Show)
    deriving anyclass (FromDhall)
