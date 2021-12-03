{-# LANGUAGE OverloadedStrings #-}

module Alma.Parser
    (Parser,
     parseMusic)
where

import Control.Applicative hiding (some)
import Control.Monad (void)
import Data.Char
import Data.Text (Text)
import Text.Megaparsec
import Text.Megaparsec.Char

import Alma.Parser.Type

-- | Space consumer parser
skipSpace :: Parser ()
skipSpace = void $! takeWhileP Nothing (== ' ')

skipSpace1 :: Parser ()
skipSpace1 = void $! takeWhile1P (Just "space") (== ' ')

skipBlankLines :: Parser ()
skipBlankLines = void $! some (skipSpace *> takeWhile1P (Just "blank line") (== '\n'))

partName :: Parser Text
partName = takeWhile1P (Just "part name") prd
  where
    prd :: Char -> Bool
    prd !c = isPrint c && not (isSpace c || c == ':')

partNameList :: Parser [Text]
partNameList = sepEndBy1 partName skipSpace1

musicLine :: Parser ([Text], Text)
musicLine = do
    ps <- partNameList
    void $! char ':' *> skipSpace1
    musicExpr <- takeWhile1P (Just "music") isPrint
    void $! newline
    pure $! (ps, musicExpr)

musicBlock :: Parser [([Text], Text)]
musicBlock = some musicLine

parseMusic :: Parser [[([Text], Text)]]
parseMusic = do
    void $! optional skipBlankLines
    sepEndBy1 musicBlock skipBlankLines
