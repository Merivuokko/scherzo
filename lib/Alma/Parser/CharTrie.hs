{-# LANGUAGE StrictData #-}
{-# LANGUAGE DerivingStrategies #-}

module Alma.Parser.CharTrie
    (CharTrie,
    empty,
    fromList,
    fromHashMap,
    insert,
    keys,
    lookup,
    null)
where

import Prelude hiding (lookup, null)

import Data.HashMap.Strict qualified as M
import Data.Hashable (Hashable)
import Data.Kind (Type)
import Data.Text (Text)
import Data.Text qualified as T

type CharTrie :: Type -> Type
newtype CharTrie a = CharTrie {
    unCharTrie :: M.HashMap Char (Maybe a, CharTrie a)
    } deriving newtype (Eq, Hashable, Monoid, Ord, Semigroup)
      deriving stock (Foldable, Functor, Read, Show, Traversable)

empty :: CharTrie a
empty = CharTrie $! M.empty

insert :: forall a. Text -> a -> CharTrie a -> CharTrie a
insert !key !val !rootTrie = snd $! go key rootTrie
  where
    go :: Text -> CharTrie a -> (Maybe a, CharTrie a)
    go !k !curTrie
        = case T.uncons k of
              Just (c, kTail) ->
                  let emptyChild = (Nothing, CharTrie $! M.empty) :: (Maybe a, CharTrie a)
                      (!childVal, !childTrie) = M.findWithDefault emptyChild c $! unCharTrie curTrie
                      !newChildTrie = go kTail childTrie
                      !newCurTrie = CharTrie $! M.insert c newChildTrie $! unCharTrie curTrie
                  in (childVal, newCurTrie)
              Nothing ->
                  (Just val, curTrie)

fromList :: forall a. [(Text, a)] -> CharTrie a
fromList [] = empty
fromList ((key, val) : xs) = insert key val $! fromList xs

fromHashMap :: M.HashMap Text v -> CharTrie v
fromHashMap = fromList . M.toList

lookup :: forall a. Char -> CharTrie a -> Maybe (Maybe a, CharTrie a)
lookup !key (CharTrie trie) = M.lookup key trie

null :: forall a. CharTrie a -> Bool
null (CharTrie trie) = M.null trie

keys :: forall a. CharTrie a -> [Char]
keys (CharTrie t) = M.keys t
