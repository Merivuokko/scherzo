module Main (main) where

import Data.Text qualified as T
import Data.Text.IO qualified as T

import Scherzo.Format.LilyPond.Reader

main :: IO ()
main = do
    input <- T.getContents
    initFileName <- getLilyPondInitFileName
    (_, out, err) <- runLilyPondExportSexp initFileName input
    let sexp = readSexp out
    T.putStrLn $! "S-expression:\n" <> (T.pack $ show sexp) <> "\nErrors: " <> err
