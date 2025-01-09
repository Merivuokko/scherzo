-- This module provides an interface for importing music in GNU LilyPond format to Scherzo's music expressions.

-- |
-- Description : LilyPond music importer for Scherzo
-- Copyright   : © 2023 Aura Kelloniemi
-- License     : GPL-3.0-only
-- Maintainer  : kaura.dev@sange.fi
module Scherzo.Format.LilyPond.Reader (
    -- * Importing already loaded data
    readLilyPond,

    -- * Reading from a file
    readLilyPondFile,
    getLilyPondInitFileName,
    runLilyPondExportSexp,

    -- * Sexp parsing
    Datum (..),
    readSexp,
) where

import Data.Text qualified as T
import Data.Text.Encoding
import Data.Text.IO qualified as T
import System.Exit (ExitCode)
import System.File.OsPath
import System.IO (hClose)
import System.OsPath
import System.Process

import Paths_scherzo_lilypond qualified as P
import Scherzo.Format.LilyPond.Reader.Datum
import Scherzo.Format.LilyPond.Reader.Parser
import Scherzo.Music.Expr

-- | Parse a LilyPond expression into MusicExpr.
readLilyPond :: T.Text -> MusicExpr
readLilyPond = undefined

-- | Read LilyPond expressions from a file, parse it and return as a MusicExpr.
-- The file is assumed to contain UTF-8 encoded text.
-- If decoding fails, an exception is thrown.
readLilyPondFile :: OsPath -> IO MusicExpr
readLilyPondFile fp = readFile' fp >>= (pure $!) . readLilyPond . decodeUtf8

-- | Get the name for thee required LilyPond init file (included with
-- scherzo-lilypond).
getLilyPondInitFileName :: IO OsPath
getLilyPondInitFileName =
    P.getDataFileName "ly/sexp-export-init.ly" >>= encodeFS

-- | Run LilyPond with proper options to convert music into a Scheme expression
--
-- Returns the LilyPond processe's exit code, stdout and stderr.
runLilyPondExportSexp
    :: OsPath
    -- ^ Path to the needed init file
    -> T.Text
    -- ^ Music in LilyPond format
    -> IO (ExitCode, T.Text, T.Text)
runLilyPondExportSexp initFp input = do
    initString <- decodeFS initFp
    (stdin, stdout, stderr, ph) <- runInteractiveProcess "lilypond" ["--init", initString, "--loglevel=WARNING", "-"] Nothing Nothing
    T.hPutStr stdin input
    hClose stdin
    outText <- T.hGetContents stdout
    errText <- T.hGetContents stderr
    status <- waitForProcess ph
    pure $! (status, outText, errText)
