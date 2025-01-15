{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- Description : Common prelude for Scherzo packages
-- Copyright   : © 2024 Aura Kelloniemi
-- License     : GPL-3.0-only
-- Maintainer  : kaura.dev@sange.fi
--
-- This module provides a Prelude replacement for Scherzo packages.
-- This mainly re-exports contents of the "Relude".
-- Some commonly useful types and utilities are re-exported from other libraries.
module Prelude (
    -- * Re-export of Relude
    module Relude,

    -- * Strict vector type
    Vector,
) where

import Data.Vector.Strict (Vector)
import Data.Vector.Strict qualified as V
import Relude

instance (Hashable a) => Hashable (Vector a) where
    hashWithSalt !salt !v = hashWithSalt salt $! V.toList v
    {-# INLINE hashWithSalt #-}
