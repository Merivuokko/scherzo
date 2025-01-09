-- |
-- Description : Music expressions
-- Copyright   : © 2023 Aura Kelloniemi
-- License     : GPL-3.0-only
-- Maintainer  : kaura.dev@sange.fi
--
-- This module defines general types and operations for describing and manipulating musical expressions.
-- MusicExpr is a recursive type similar to an abstract syntax tree.

module Scherzo.Music.Expr
    (
        -- * Music expressions
        MusicExpr (..),
        flattenMusic,

        -- * Musical time calculation
        musicExprLength,
    ) where

import Data.Foldable (foldl')
import Data.Hashable
import GHC.Generics (Generic)

import Scherzo.Music.Elementary

-- | Musical expression
data MusicExpr
    -- | Sequentially progressing music
    = SequentialExpr [MusicExpr]
    -- | Simultaneously sounding music
    | SimultaneousExpr [MusicExpr]
    -- | A single note
    | NoteExpr Note
    -- | A rest
    | RestExpr Rest
    -- | A barline
    | BarExpr
    deriving stock (Eq, Generic, Show)
    deriving anyclass Hashable

-- | Remove unnecessary nesting from a music expression
flattenMusic :: MusicExpr -> MusicExpr
flattenMusic (SequentialExpr ms) = SequentialExpr $! unseq ms
  where
    unseq :: [MusicExpr] -> [MusicExpr]
    unseq [] = []
    unseq ((SequentialExpr x) : xs) = unseq x <> unseq xs
    unseq (x : xs) = x : unseq xs
flattenMusic ms = ms

-- | Calculate music expressiong length in musical time
-- O(n)
musicExprLength :: MusicExpr -> MusicLength
musicExprLength = \case
    SequentialExpr xs -> foldl' (flip $! (+) . musicExprLength) 0 xs
    SimultaneousExpr xs -> maximum $! fmap musicExprLength xs
    NoteExpr n -> durationLength $! n.duration
    RestExpr r -> durationLength $! r.duration
    BarExpr -> 0
