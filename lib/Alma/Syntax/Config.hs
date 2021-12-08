{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StrictData #-}

-- | Syntax command definitions

module Alma.Syntax.State
    (SStateName,
    SState,
    Command)
where

import Data.Kind (Type)
import Dhall

type SStateName :: Type
type SStateName = Text

type SState :: Type
data SState = SState {
    init :: Command,
    fail :: Command,
    tokens :: [Command]
    }
    deriving stock (Eq, Generic, Ord, Read, Show)
    deriving anyclass (FromDhall)
                      
type Command :: Type
data Command
    = EnterState SStateName
    | LeaveState
    | Finish
    | Fail
    | Write Text
    deriving stock (Eq, Generic, Ord, Read, Show)
    deriving anyclass (FromDhall)
