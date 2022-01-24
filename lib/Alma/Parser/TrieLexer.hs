-- | Parse a music token using a CharTrie

module Alma.Parser.TrieLexer
    (parseWithTrie)
where

import Control.Applicative
import Data.List.NonEmpty qualified as NL
import Data.Set qualified as Set
import Text.Megaparsec

import Alma.Parser.CharTrie qualified as CT
import Alma.Parser.Type

parseWithTrie :: forall a. CT.CharTrie a -> Parser a
{-# INLINEABLE parseWithTrie #-}
parseWithTrie !trie = go trie
  where
    go :: CT.CharTrie a -> Parser a
    {-# INLINEABLE go #-}
    go !t = do
        ch <- lookAhead anySingle
        case CT.lookup ch t of
            Just (nextVal, nextTrie) ->
                anySingle *> case nextVal of
                    Just res -> do
                        let recursive = if CT.null nextTrie then empty else try $! go nextTrie
                        recursive <|> (pure $! res)
                    Nothing -> do
                        go nextTrie
            Nothing -> do
                errorOut ch t
    errorOut :: Char -> CT.CharTrie a -> Parser a
    {-# INLINEABLE errorOut #-}
    errorOut !ch !t
        | CT.null t = error $! "Programming error: No tokens to parse"
        | otherwise = failure (Just $! Tokens $! NL.singleton ch) expectedTokens
      where
        expectedTokens :: Set.Set (ErrorItem Char)
        {-# INLINEABLE expectedTokens #-}
        expectedTokens = Set.fromList $! fmap charToErrorItem $! CT.keys t
        charToErrorItem :: Char -> ErrorItem Char
        {-# INLINEABLE charToErrorItem #-}
        charToErrorItem !c = Tokens $! NL.singleton c
