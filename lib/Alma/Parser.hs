-- | The Alma parser

module Alma.Parser
    (Parser,
     readConfigFile,
     testParser)
where

import Control.Monad.Reader
import Data.Text (Text)
import Text.Megaparsec

import Alma.Parser.Config
import Alma.Parser.Rules
import Alma.Parser.Type

testParser :: ParserConfig -> Text -> IO ()
testParser !config !input = parseTest (runReaderT parseMusic config) input
