-- | Types shared between Alma.Parser submodules

module Alma.Parser.Type
    (Parser)
where

import Control.Monad.Reader
import Data.Kind (Type)
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (Parsec)

import Alma.Syntax.Config

type Parser :: Type -> Type
type Parser = ReaderT SyntaxConfig (Parsec Void Text)
