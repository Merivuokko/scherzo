-- | The Alma parser

module Alma.Parser
    (Parser,
     ParserConfig,
     readConfigFile,
     parseMusic)
where

import Alma.Parser.Config
import Alma.Parser.Rules
import Alma.Parser.Type
