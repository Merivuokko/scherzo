{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      : Scherzo.Music
-- Description : Types and utilities for elementary music description
-- Copyright   : Copyright (C) 2021 Aura Kelloniemi
-- License     : GPL-3
-- Maintainer  : kaura.dev@sange.fi
-- Stability   : experimental
-- Portability : undefined
-- 
-- This module defines general types and operations for describing musical notation.

module Scherzo.Music (
    -- * Notes and rests
    Note (..),
    Rest (..),
    NoteDuration (..),
    NoteValue (..),
    PitchClass (..),
    Octave,
    Alteration (..),
    Articulation (..),
    staffPositionsBetween,

    -- * Music expressions
    MusicExpr (..),
    flattenMusic,

    -- * Musicaltime calculation
    MusicLength,
    musicExprLength,
    noteLength,
    valueLength
    ) where

import Data.Foldable (foldl')
import Data.Kind (Type)
import Data.Ratio

-- | Scientific octave number
type Octave :: Type
type Octave = Int

-- | A musical note
type Note :: Type
data Note = Note {
    pitch :: PitchClass,
    alteration :: Alteration,
    octave :: Octave,
    duration :: NoteDuration,
    articulations :: [Articulation]
    } deriving stock (Eq, Show)

-- | A musical rest
type Rest :: Type
data Rest = Rest {
    duration :: NoteDuration,
    articulations :: [Articulation]
    } deriving stock (Eq, Show)

-- | Pitch class is a set of pitches associated with a musical scale. FOr each pitch class there is a single pitch (frequency) within each octave.
type PitchClass :: Type
data PitchClass = C | D | E | F | G | A | B
                deriving stock (Eq, Enum, Ord, Show)

-- | Alternation of a pitch
type Alteration :: Type
data Alteration = DoubleFlat
                | Flat
                | Natural
                | Sharp
                | DoubleSharp
                deriving stock (Eq, Ord, Show)

staffPositionsBetween :: Note -> Note -> Int
staffPositionsBetween a b
    = (a.octave * 7 + fromEnum a.pitch) - (b.octave * 7 + fromEnum b.pitch)

-- | Duration definition of a note or a rest
type NoteDuration :: Type
data NoteDuration = NoteDuration {
    value :: NoteValue,
    augmentationDots :: Int,
    durationScaling :: Ratio Int
    } deriving stock (Eq, Show)

-- | Note head types
type NoteValue :: Type
data NoteValue = Maxima
               | Longa
               | Breve
               | Semibreve
               | Minim
               | Crotchet
               | Quaver
               | Semiquaver
               | A32th
               | A64th
               | A128th
               | A256th
               deriving stock (Eq, Ord, Show)

-- | Musical expression
type MusicExpr :: Type
data MusicExpr = SequentialExpr [MusicExpr]
                 -- ^ Sequentially progressing music
               | NoteExpr Note
                 -- ^ A single note
               | ChordExpr [Note]
                 -- ^ Simultaneous notes
               | RestExpr Rest
                 -- ^ A single rest
               | BarExpr
                 -- ^ An explicit barline
               deriving stock (Eq, Show)
-- | Remove unnecessary nesting from a music expression
flattenMusic :: MusicExpr -> MusicExpr
flattenMusic (SequentialExpr ms) = SequentialExpr $! unseq ms
  where
    unseq :: [MusicExpr] -> [MusicExpr]
    unseq [] = []
    unseq ((SequentialExpr x) : xs) = unseq x ++ unseq xs
    unseq (x : xs) = x : unseq xs
flattenMusic ms = ms

-- | Length of a music expression in musical time
type MusicLength :: Type
type MusicLength = Ratio Int

valueLength :: NoteValue -> MusicLength
valueLength v
    = case v of
          Maxima -> 8
          Longa -> 4
          Breve -> 2
          Semibreve -> 1
          Minim -> 1 % 2
          Crotchet -> 1 % 4
          Quaver -> 1 % 8
          Semiquaver -> 1 % 16
          A32th -> 1 % 32
          A64th -> 1 % 64
          A128th -> 1 % 128
          A256th -> 1 % 256

noteLength :: NoteDuration -> MusicLength
noteLength nd = nd.durationScaling * (baseLength + augment nd.augmentationDots)
  where
    baseLength :: MusicLength
    baseLength = valueLength nd.value

    augment :: Int -> MusicLength
    augment dots =
        let !denominator = 2 ^ dots
            !numerator = denominator - 1
        in baseLength * (numerator % denominator)

-- | Calculate music expressiong length in musical time
-- O(n)
musicExprLength :: MusicExpr -> MusicLength
musicExprLength (SequentialExpr xs) = foldl' (flip $! (+) . musicExprLength) 0 xs
-- musicExprLength (SimultaneousExpr xs ) = maximum $! map musicExprLength xs
musicExprLength (NoteExpr n) = noteLength $! n.duration
musicExprLength (ChordExpr ns) = maximum . map (noteLength . (.duration)) $! ns
musicExprLength (RestExpr r) = noteLength $! r.duration
musicExprLength _ = 0

-- | An articulation event associated with a note or rest
type Articulation :: Type
data Articulation = Articulation
    deriving stock (Eq, Show)
