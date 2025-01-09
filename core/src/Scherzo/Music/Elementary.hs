-- |
-- Description : Basic music element definitions
-- Copyright   : © 2023 Aura Kelloniemi
-- License     : GPL-3.0-only
-- Maintainer  : kaura.dev@sange.fi
--
-- This module defines elementary components of musical notation:
-- note pitches, values, octaves, etc. and utility functions for working with them.
module Scherzo.Music.Elementary (
    -- * Note pitch
    PitchName (..),
    Alteration (..),
    Octave,
    NotePitch (..),
    staffPositionsBetween,

    -- * Note duration
    NoteValue (..),
    AugmentationDotCount,
    NoteDuration (..),

    -- * Expressive marks
    Articulation (..),

    -- * Notes and rests
    Note (..),
    RestType (..),
    Rest (..),

    -- * Musical time calculation
    MusicLength,
    valueLength,
    lengthToValue,
    durationLength,
    lengthToDuration,
) where

import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import GHC.Real (Ratio (..), (%))
import Numeric.Logarithms

-- | Pitch name refers to a pitch of a note within one octave.
-- This is terminology most commonly used in tonal music.
-- Set theory talks about pitch classes.
-- English note names are used in scherzo.
data PitchName
    = C
    | D
    | E
    | F
    | G
    | A
    | B
    deriving stock (Bounded, Enum, Eq, Ord, Generic, Show)
    deriving anyclass (Hashable)

-- | Alternation of a pitch
data Alteration
    = DoubleFlat
    | Flat
    | Natural
    | Sharp
    | DoubleSharp
    deriving stock (Eq, Generic, Ord, Show)
    deriving anyclass (Hashable)

-- | Scientific octave number.
-- Value of 4 refers to the one-lined octave, where middle C resides.
type Octave = Int

-- | Absolute pitch of a note
data NotePitch = NotePitch
    { name :: PitchName,
      alteration :: Alteration,
      octave :: Octave
    }
    deriving stock (Eq, Generic, Show)
    deriving anyclass (Hashable)

-- | Count the number of staff positions between two pitches.
-- This is a limited view of an interval.
-- 0 = prime, 1 = second, 2 = third, etc.
staffPositionsBetween :: NotePitch -> NotePitch -> Int
staffPositionsBetween a b =
    (a.octave * 7 + fromEnum a.name) - (b.octave * 7 + fromEnum b.name)

-- | Note head types
data NoteValue
    = -- | Eight times whole note
      Maxima
    | -- | Four times whole note
      Longa
    | -- | Two times whole note
      Breve
    | -- | Whole note
      Semibreve
    | -- | Half note
      Minim
    | -- | Quarter note
      Crotchet
    | -- | Eighth note
      Quaver
    | -- | Sixteenth note
      Semiquaver
    | -- | 32th note
      Demisemiquaver
    | -- | 64th note
      Hemidemisemiquaver
    | -- | 128th note
      A128th
    | -- | 256th note
      A256th
    | -- | 512th note
      A512th
    | -- | 1024th note
      A1024th
    deriving stock (Bounded, Enum, Eq, Generic, Ord, Show)
    deriving anyclass (Hashable)

-- | Number of augmentation dots affecting a note value
type AugmentationDotCount = Int

-- | Duration of a note or a rest
data NoteDuration = NoteDuration
    { value :: NoteValue,
      dots :: AugmentationDotCount
    }
    deriving stock (Eq, Generic, Show)
    deriving anyclass (Hashable)

-- | An articulation event associated with a note or rest.
-- (This is a stub to be expanded later.)
data Articulation = Articulation
    deriving stock (Eq, Generic, Show)
    deriving anyclass (Hashable)

-- | A single musical note or a chord.
-- It defines a single-pithced note with duration or a chord with multiple tones, but each with the same duration.
data Note = Note
    { pitches :: [NotePitch],
      duration :: NoteDuration,
      articulations :: [Articulation]
    }
    deriving stock (Eq, Generic, Show)
    deriving anyclass (Hashable)

-- | Type of a rest
data RestType
    = -- | Normal printed rest with an optional staff position definition
      NormalRest (Maybe (PitchName, Octave))
    | -- | A multi-measure rest
      MultiMeasureRest
    | -- An invisible spacer rest
      SpacerRest
    deriving stock (Eq, Generic, Show)
    deriving anyclass (Hashable)

-- | A musical rest
data Rest = Rest
    { restType :: RestType,
      duration :: NoteDuration,
      articulations :: [Articulation]
    }
    deriving stock (Eq, Generic, Show)
    deriving anyclass (Hashable)

-- | Length of a music expression in musical time
type MusicLength = Ratio Int

-- | Calculate the length of a note with a given note value
valueLength :: NoteValue -> MusicLength
valueLength = \case
    Maxima -> 8
    Longa -> 4
    Breve -> 2
    Semibreve -> 1
    Minim -> 1 % 2
    Crotchet -> 1 % 4
    Quaver -> 1 % 8
    Semiquaver -> 1 % 16
    Demisemiquaver -> 1 % 32
    Hemidemisemiquaver -> 1 % 64
    A128th -> 1 % 128
    A256th -> 1 % 256
    A512th -> 1 % 512
    A1024th -> 1 % 1024

-- | If possible, determine the note value from its length.
-- If the length is not representable by a basic note value, Nothing is returned.
lengthToValue :: MusicLength -> Maybe NoteValue
lengthToValue = \case
    -- Beware that the (:%) constructor does not reduce fractions, and thus the numbers below need to be in their most reduced form.
    8 :% 1 -> Just Maxima
    4 :% 1 -> Just Longa
    2 :% 1 -> Just Breve
    1 :% 1 -> Just Semibreve
    1 :% 2 -> Just Minim
    1 :% 4 -> Just Crotchet
    1 :% 8 -> Just Quaver
    1 :% 16 -> Just Semiquaver
    1 :% 32 -> Just Demisemiquaver
    1 :% 64 -> Just Hemidemisemiquaver
    1 :% 128 -> Just A128th
    1 :% 256 -> Just A256th
    1 :% 512 -> Just A512th
    1 :% 1024 -> Just A1024th
    _ -> Nothing

-- | Calculate the length of a NoteDuration.
durationLength :: NoteDuration -> MusicLength
durationLength nd = baseLength + baseLength * augment nd.dots
  where
    baseLength :: MusicLength
    baseLength = valueLength nd.value

    augment :: Int -> MusicLength
    augment dots =
        let !denom = 2 ^ dots
            !numer = denom - 1
        in  numer % denom

-- | If possible, determine the duration of a single note given its length.
-- If the given length does not represent a duration expressible with note values and augmentation dots, return Nothing.
lengthToDuration :: MusicLength -> Maybe NoteDuration
lengthToDuration len =
    let baseLen = 2 ^^ log2Floor len
        filledTime = 2 * baseLen
        (negDots, ord) = log2Approx ((filledTime - len) / baseLen)
    in  case (ord, lengthToValue baseLen) of
            (EQ, Just value) ->
                Just $!
                    NoteDuration
                        { value = value,
                          dots = (-1) * negDots
                        }
            _ -> Nothing
