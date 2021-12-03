-- | Types shared between Alma.Parser submodules

module Alma.Parser.Type
    (Parser)
where

import Data.Kind (Type)
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (Parsec)

type Parser :: Type -> Type
type Parser = Parsec Void Text
