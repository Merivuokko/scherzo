-- |
-- Module      : Scherzo.Format.LilyPond.Reader.Datum
-- Description : A simplified representation of Guile s-expression
-- Copyright   : Copyright (C) 2023-2024 Aura Kelloniemi
-- License     : GPL-3
-- Maintainer  : kaura.dev@sange.fi
-- Stability   : experimental
-- Portability : GHC
--
-- This module provides a data type for representing simplified s-expresion
-- syntax as produced by Guile's pretty-print library.
--
-- Data in this format is exported from within LilyPond using Scherzo's
-- sexp-export library.
module Scherzo.Format.LilyPond.Reader.Datum (
    -- * Scheme data representation
    Datum (..),
) where

import Data.Text qualified as T

-- | A basic scheme value
data Datum
    = Bool Bool
    | Char Char
    | Number Int
    | String T.Text
    | Symbol T.Text
    | -- | The Bool in the List constructor indicates whether the list is a proper list (i.e. not dotted)
      List Bool [Datum]
    deriving stock (Eq, Show)
