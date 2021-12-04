-- | Parse a music token using a CharTrie

module Alma.Parser.TrieLexer
    (parseWithTrie)
where

import Control.Applicative
import Data.List.NonEmpty qualified as NL
import Data.Set qualified as Set
import Text.Megaparsec

import Alma.Parser.CharTrie as CT
import Alma.Parser.Type

parseWithTrie :: forall a. CT.CharTrie a -> Parser a
{-# INLINEABLE parseWithTrie #-}
parseWithTrie !trie
    | CT.null trie = error $! "Programming error: Cannot parse with no tokens"
    | otherwise = go trie
  where
    go :: CT.CharTrie a -> Parser a
    {-# INLINEABLE go #-}
    go !t = do
        ch <- lookAhead anySingle
        case CT.lookup ch t of
            Just (nextVal, nextTrie) ->
                anySingle *> case nextVal of
                    Just res ->
                        try (go nextTrie) <|> (pure $! res)
                    Nothing ->
                        go nextTrie
            Nothing ->
                errorOut ch t
    errorOut :: Char -> CT.CharTrie a -> Parser a
    {-# INLINEABLE errorOut #-}
    -- Note:return value of errorOut should be a thunk so that it is not
    -- evaluated in case parsing succeeded on a previous level of the trie.
    errorOut !ch !t
        = failure (Just $! Tokens $! NL.singleton ch) expectedTokens
      where
        expectedTokens :: Set.Set (ErrorItem Char)
        {-# INLINEABLE expectedTokens #-}
        expectedTokens =
            if CT.null t
            then error $! "Programming error: Dead end in parsing trie"
            else Set.fromList $! map charToErrorItem $! CT.keys t
        charToErrorItem :: Char -> ErrorItem Char
        {-# INLINEABLE charToErrorItem #-}
        charToErrorItem !c = Tokens $! NL.singleton c
