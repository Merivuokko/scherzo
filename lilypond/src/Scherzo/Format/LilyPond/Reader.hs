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

import Data.ByteString qualified as BS
import System.Exit (ExitCode)
import System.IO (hClose)
import System.OsPath
import System.Process

import Paths_scherzo_lilypond qualified as P
import Scherzo.Format.LilyPond.Reader.Datum
import Scherzo.Format.LilyPond.Reader.Parser
import Scherzo.Music.Expr

-- | Parse a LilyPond expression into MusicExpr.
readLilyPond :: Text -> MusicExpr
readLilyPond = undefined

-- | Read LilyPond expressions from a file, parse it and return as a MusicExpr.
-- The file is assumed to contain UTF-8 encoded text.
-- If decoding fails, an exception is thrown.
readLilyPondFile :: OsPath -> IO MusicExpr
readLilyPondFile _fp = undefined

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
    -> Either OsPath Text
    -- ^ Either a LilyPond score file name or music in LilyPond format
    -> IO (ExitCode, Text, Text)
runLilyPondExportSexp initFp input = do
    initPathString <- decodeFS initFp
    (musicPathString, musicText) <- case input of
        Left path -> do
            pathS <- decodeFS path
            pure (pathS, "")
        Right text -> pure ("-", text)
    (stdinHandle, stdoutHandle, stderrHandle, procHandle) <- runInteractiveProcess "lilypond" ["--init", initPathString, "--loglevel=WARNING", musicPathString] Nothing Nothing
    BS.hPut stdinHandle (encodeUtf8 musicText)
    hClose stdinHandle
    outText <- decodeUtf8 <$> BS.hGetContents stdoutHandle
    errText <- decodeUtf8 <$> BS.hGetContents stderrHandle
    status <- waitForProcess procHandle
    pure (status, outText, errText)
