module Main (main) where

import Data.Text.IO qualified as T

import Scherzo.Format.LilyPond.Reader

main :: IO ()
main = do
    input <- T.getContents
    initFileName <- getLilyPondInitFileName
    (_, out, err) <- runLilyPondExportSexp initFileName (Right input)
    let sexp = readSexp out
    putTextLn $! "S-expression:\n" <> show sexp <> "\nErrors: " <> err
