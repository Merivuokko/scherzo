module Main (main) where

import Data.Text qualified as T
import Data.Text.IO qualified as T
import System.OsPath

import Scherzo.Format.LilyPond.Reader

main :: IO ()
main = do
    text <- T.getContents
    (_, out, err) <-
        runLilyPondExportSexp
            [osp|/home/aura/proj/dev/scherzo/lilypond/ly/sexp-export-init.ly|]
            text
    let sexp = readSexp out
    T.putStrLn $! "S-expression:\n" <> (T.pack $ show sexp) <> "\nErrors: " <> err
