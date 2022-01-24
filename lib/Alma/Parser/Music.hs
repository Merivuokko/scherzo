{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Parser state machine

module Alma.Parser.Music
    (parseMusicExpr)
where

import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.HashMap.Strict qualified as HM
import Data.Kind (Type)
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T

import Alma.Parser.TrieLexer
import Alma.Parser.Type
import Alma.Syntax.Config

type StatefulParser :: Type -> Type -> Type
type StatefulParser s a = StateT s Parser a

type MusicParserState :: Type
data MusicParserState = MusicParserState {
    contextStack :: NE.NonEmpty ContextDef,
    output :: T.Text
    }
    deriving stock (Eq, Show)

initialMusicParserState :: ContextDef -> MusicParserState
initialMusicParserState !ctx = MusicParserState {
    contextStack = NE.singleton ctx,
    output = ""
    }

type MusicParser :: Type -> Type
type MusicParser a = StatefulParser MusicParserState a

embedMusicParser :: MusicParser a -> Parser MusicParserState
embedMusicParser p = do
    config <- ask
    case HM.lookup config.initContext config.contexts of
        Just ctx -> do
            s <- execStateT p (initialMusicParserState ctx)
            pure s
        Nothing ->
            fail $ "Invalid parser state: " <> T.unpack config.initContext

parseMusicExpr :: Parser T.Text
parseMusicExpr = do
    st <- embedMusicParser go
    pure $! st.output
  where
    go :: MusicParser ()
    go = do
        ctx <- gets (NE.head . (.contextStack))
        cmds <- (lift . parseWithTrie) ctx.tokens.trie
        done <- dispatch cmds
        if done
            then pure ()
            else go

    dispatch :: [Command] -> MusicParser Bool
    dispatch [] = pure False
    dispatch (cmd : cmds) = do
        done <- parseMusicCommand cmd
        if done
            then pure True
            else dispatch cmds

parseMusicCommand :: Command -> MusicParser Bool
parseMusicCommand (Enter name) = enterContext name
parseMusicCommand Leave = leaveContext
parseMusicCommand Finish = pure True
parseMusicCommand (Fail msg) = fail $! T.unpack msg
parseMusicCommand (Write t) = writeText t

enterContext :: ContextName -> MusicParser Bool
enterContext name = do
    contexts <- asks (.contexts)
    context <- case HM.lookup name contexts of
        Just ctx -> pure ctx
        Nothing -> fail $! "Unknown context name: " <> T.unpack name
    modify' \s -> s { contextStack = NE.cons context s.contextStack }
    pure False

leaveContext :: MusicParser Bool
leaveContext = do
    modify' \s -> s { contextStack = NE.fromList $! NE.tail $! s.contextStack }
    pure False

writeText :: T.Text -> MusicParser Bool
writeText t = do
    modify' \s -> s { output = s.output <> " | " <> t }
    pure False
