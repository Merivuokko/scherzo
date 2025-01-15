-- This module provides facilities for exporting Scherzo music expressions to
-- GNU LilyPond format.

-- |
-- Description : LilyPond music export routines for Scherzo
-- Copyright   : © 2023 Aura Kelloniemi
-- License     : GPL-3.0-only
-- Maintainer  : kaura.dev@sange.fi
module Scherzo.Format.LilyPond.Writer (
    musicToLilyPond,
) where

import Data.Text qualified as T
import Data.Vector.Strict qualified as V

import Scherzo.Music.Elementary
import Scherzo.Music.Expr

durationToLilyPond :: NoteDuration -> Text
durationToLilyPond duration = value duration.value <> dots duration.dots
  where
    dots :: Int -> Text
    dots = flip T.replicate "."

    value :: NoteValue -> Text
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

musicToLilyPond :: MusicExpr -> Text
musicToLilyPond = unwords . go . flattenMusic
  where
    go :: MusicExpr -> [Text]
    go (SequentialExpr xs) = ["{", unwords (unwords . go <$> V.toList xs), "}"]
    go (SimultaneousExpr xs) = ["<<", unwords (unwords . go <$> V.toList xs), ">>"]
    go (NoteExpr note) = [noteToLilyPond note]
    go (RestExpr rest) = [restToLilyPond rest]
    go BarExpr = ["|"]

noteToLilyPond :: Note -> Text
noteToLilyPond note = pitches note.pitches <> durationToLilyPond note.duration
  where
    alteration :: Alteration -> Text
    alteration = \case
        DoubleFlat -> "eses"
        Flat -> "es"
        Natural -> ""
        Sharp -> "is"
        DoubleSharp -> "isis"

    notePitch :: NotePitch -> Text
    notePitch p = pitchNameToLilyPond p.name <> alteration p.alteration <> octaveToLilyPond p.octave

    pitches :: Vector NotePitch -> Text
    pitches ps = case V.length ps of
        0 -> "<>"
        1 -> notePitch . V.unsafeHead $! ps
        _ -> "<" <> (unwords . V.toList . fmap notePitch $! ps) <> ">"

octaveToLilyPond :: Octave -> Text
octaveToLilyPond oct
    | oct == 3 = ""
    | oct > 3 = T.replicate (oct - 3) "'"
    | otherwise = T.replicate (3 - oct) ","

pitchNameToLilyPond :: PitchName -> Text
pitchNameToLilyPond = \case
    C -> "c"
    D -> "d"
    E -> "e"
    F -> "f"
    G -> "g"
    A -> "a"
    B -> "b"

restToLilyPond :: Rest -> Text
restToLilyPond rest = restType rest.restType <> durationToLilyPond rest.duration
  where
    restType :: RestType -> Text
    restType = \case
        NormalRest maybePitch ->
            case maybePitch of
                Nothing -> "r"
                Just (pitch, octave) -> pitchNameToLilyPond pitch <> octaveToLilyPond octave <> "\\rest"
        MultiMeasureRest -> "R"
        SpacerRest -> "s"
