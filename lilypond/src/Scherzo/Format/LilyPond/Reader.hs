-- This module provides an interface for importing music in GNU LilyPond format to Scherzo's music expressions.

-- |
-- Module      : Scherzo.Format.LilyPond.Reader
-- Description : LilyPond music importer for Scherzo
-- Copyright   : Copyright (C) 2023-2024 Aura Kelloniemi
-- License     : GPL-3
-- Maintainer  : kaura.dev@sange.fi
-- Stability   : experimental
-- Portability : GHC
module Scherzo.Format.LilyPond.Reader (
    -- * Importing already loaded data
    readLilyPond,

    -- * Reading from a file
    readLilyPondFile,
) where

import Data.Text qualified as T
import Data.Text.Encoding
import System.File.OsPath
import System.OsPath

import Scherzo.Music.Expr

-- | Parse a LilyPond expression into MusicExpr.
readLilyPond :: T.Text -> MusicExpr
readLilyPond = undefined

-- | Read LilyPond expressions from a file, parse it and return as a MusicExpr.
-- The file is assumed to contain UTF-8 encoded text.
-- If decoding fails, an exception is thrown.
readLilyPondFile :: OsPath -> IO MusicExpr
readLilyPondFile fp = readFile' fp >>= (pure $!) . readLilyPond . decodeUtf8
