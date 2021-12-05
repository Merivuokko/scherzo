{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Alma parser rules

module Alma.Parser.Rules
    (parseMusic)
where

import Control.Applicative hiding (some)
import Control.Monad.Reader
import Data.Char
import Data.Text (Text)
import Text.Megaparsec
import Text.Megaparsec.Char

import Alma.Parser.TrieLexer
import Alma.Parser.Type
import Alma.Syntax.Config

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

musicLine :: Parser ([Text], [Text])
musicLine = do
    ps <- partNameList
    void $! char ':' *> skipSpace1
    expr <- musicExpr
    pure $! (ps, expr)

musicExpr :: Parser [Text]
musicExpr = do
    config <- ask
    manyTill (parseWithTrie config.tokenTrie) newline
    
musicBlock :: Parser [([Text], [Text])]
musicBlock = some musicLine

parseMusic :: Parser [[([Text], [Text])]]
parseMusic = do
    void $! optional skipBlankLines
    sepEndBy1 musicBlock skipBlankLines
