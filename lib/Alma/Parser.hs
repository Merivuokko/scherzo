-- | The Alma parser

module Alma.Parser
    (Parser,
     testParser)
where

import Control.Monad.Reader
import Data.Text (Text)
import Text.Megaparsec

import Alma.Parser.Rules
import Alma.Parser.Type
import Alma.Syntax.Config

testParser :: SyntaxConfig -> Text -> IO ()
testParser !config !input = parseTest (runReaderT parseMusic config) input
