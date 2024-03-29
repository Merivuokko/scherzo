-- |
-- Module      : Scherzo.Format.LilyPond.Writer
-- Description : LilyPond music export routines for Scherzo
-- Copyright   : Copyright (C) 2023-2024 Aura Kelloniemi
-- License     : GPL-3
-- Maintainer  : kaura.dev@sange.fi
-- Stability   : experimental
-- Portability : GHC

-- This module provides facilities for exporting Scherzo music expressions to
-- GNU LilyPond format.
module Scherzo.Format.LilyPond.Writer (
    musicToLilyPond,
) where

import Data.Text qualified as T

import Scherzo.Music.Elementary
import Scherzo.Music.Expr

durationToLilyPond :: NoteDuration -> T.Text
durationToLilyPond duration = value duration.value <> dots duration.dots
  where
    dots :: Int -> T.Text
    dots = flip T.replicate "."

    value :: NoteValue -> T.Text
    value = \case
        Maxima -> "\\maxima"
        Longa -> "\\longa"
        Breve -> "\\breve"
        Semibreve -> "1"
        Minim -> "2"
        Crotchet -> "4"
        Quaver -> "8"
        Semiquaver -> "16"
        Demisemiquaver -> "32"
        Hemidemisemiquaver -> "64"
        A128th -> "128"
        A256th -> "256"
        A512th -> "512"
        A1024th -> "1024"

musicToLilyPond :: MusicExpr -> T.Text
musicToLilyPond = T.unwords . go . flattenMusic
  where
    go :: MusicExpr -> [T.Text]
    go (SequentialExpr xs) = [ "{", T.unwords (fmap (T.unwords . go) xs), "}" ]
    go (SimultaneousExpr xs) = [ "<<", T.unwords (fmap (T.unwords . go) xs), ">>" ]
    go (NoteExpr note) = [noteToLilyPond note]
    go (RestExpr rest) = [restToLilyPond rest]
    go BarExpr = [ "|" ]

noteToLilyPond :: Note -> T.Text
noteToLilyPond note = pitches note.pitches <> durationToLilyPond note.duration
  where
    alteration :: Alteration -> T.Text
    alteration = \case
        DoubleFlat -> "eses"
        Flat -> "es"
        Natural -> ""
        Sharp -> "is"
        DoubleSharp -> "isis"

    notePitch :: NotePitch -> T.Text
    notePitch p = pitchNameToLilyPond p.name <> alteration p.alteration <> octaveToLilyPond p.octave

    pitches :: [NotePitch] -> T.Text
    pitches [pitch] = notePitch pitch
    pitches [] = "<>"
    pitches ps = "<" <> T.unwords (map notePitch ps) <> ">"

octaveToLilyPond :: Octave -> T.Text
octaveToLilyPond oct
    | oct == 3 = ""
    | oct > 3 = T.replicate (oct - 3) "'"
    | otherwise = T.replicate (3 - oct) ","

pitchNameToLilyPond :: PitchName -> T.Text
pitchNameToLilyPond = \case
    C -> "c"
    D -> "d"
    E -> "e"
    F -> "f"
    G -> "g"
    A -> "a"
    B -> "b"

restToLilyPond :: Rest -> T.Text
restToLilyPond rest = restType rest.restType <> durationToLilyPond rest.duration
  where
    restType :: RestType -> T.Text
    restType = \case
        NormalRest maybePitch ->
            case maybePitch of
                Nothing -> "r"
                Just (pitch, octave) -> pitchNameToLilyPond pitch <> octaveToLilyPond octave <> "\\rest"
        MultiMeasureRest -> "R"
        SpacerRest -> "s"
