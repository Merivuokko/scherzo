{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StrictData #-}

-- | Syntax configuration

module Alma.Syntax.Config
    (SyntaxConfig (..),
     TokenMap (map, trie),
     readConfigFile,
     ContextName,
     ContextDef,
     Command)
where

import Data.Kind (Type)
import Data.HashMap.Strict qualified as HM
import Data.Text qualified as T
import Dhall

import Alma.Parser.CharTrie as CT

type SyntaxConfig :: Type
data SyntaxConfig = SyntaxConfig {
    contexts :: HM.HashMap ContextName ContextDef,
    initContext :: ContextName
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

type ContextName :: Type
type ContextName = T.Text

type ContextDef :: Type
data ContextDef = ContextDef {
    init :: Command,
    fail :: Command,
    tokens :: TokenMap Command
    }
    deriving stock (Eq, Generic, Show)
    deriving anyclass (FromDhall)
                      
type Command :: Type
data Command
    = EnterContext SContextName
    | LeaveContext
    | Finish
    | Fail
    | Write T.Text
    deriving stock (Eq, Generic, Ord, Show)
    deriving anyclass (FromDhall)
