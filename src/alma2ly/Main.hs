{-# LANGUAGE OverloadedStrings #-}

module Main
    (main)
where

import Data.Text.IO qualified as T

import Alma.Parser

main :: IO ()
main = do
    T.putStrLn "Type thine program."
    input <- T.getContents
    config <- readConfigFile "test.dhall"
    testParser config input
