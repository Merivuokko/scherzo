-- |
-- Description : A parser for basic Guile s-expressions
-- Copyright   : © 2023 Aura Kelloniemi
-- License     : GPL-3.0-only
-- Maintainer  : kaura.dev@sange.fi
--
-- This module provides a parser for simplified s-expresion syntax as produced
-- by Guile's pretty-print library.
--
-- This is not a complete Guile or Scheme parser. It understands the minimum
-- required subset of Guile's s-expression syntax that is needed to import
-- LilyPond music to Scherzo.
module Scherzo.Format.LilyPond.Reader.Parser (
    -- * Parsing
    readSexp,
) where

import Prelude hiding (many)

import Data.Char (isDigit)
import Data.Map.Strict qualified as M
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Text.Read qualified as T
import Text.Megaparsec

import Scherzo.Format.LilyPond.Reader.Datum

-- | Type of our parser
type Parser = Parsec Void Text

-- | Parse a Scheme expression
readSexp
    :: Text
    -- ^ Input text
    -> Either Text Datum
readSexp input =
    case runParser parseDatum "<scheme>" input of
        Left err -> Left . toText . errorBundlePretty $ err
        Right result -> Right result

-- | Parse a basic or compound datum
parseDatum :: Parser Datum
parseDatum =
    parseString
        <|> parseList
        <|> parseHashPrefixed
        <|> parseNumber
        <|> parseSymbol

parseNumber :: Parser Datum
parseNumber = do
    (text, _) <- try $ match (optional (single '+' <|> single '-') *> parseSeparated (takeWhile1P (Just "digits") isDigit))
    void $! spaces
    case T.signed T.decimal text of
        Left err -> fail $ "Error while reading a number: " <> err
        Right (num, _) -> pure $! Number num

-- | Parse a symbol. This is currently very quick an dirty implementation. It
-- considers all but the separator characters to be part of a symbol name.
parseSymbol :: Parser Datum
parseSymbol =
    -- Beware of the dot
    try $! do
        text <- takeWhile1P (Just "symbol") (not . isSeparator) <* spaces
        if text == "."
            then fail "Dot is not a symbol"
            else pure $! Symbol text

-- | Parse a list. Both proper (normal) and improper (dotted) lists are
-- recognized.
parseList :: Parser Datum
parseList = do
    void $ single '(' *> spaces
    initial <- many parseDatum
    let proper = pure $ List True initial
        improper = do
            try $! single '.' *> spaces1
            final <- parseDatum
            pure $! List False (initial ++ [final])
    result <- improper <|> proper
    void $! single ')' *> spaces
    pure $! result

-- | Parse anything starting with a hash sign
parseHashPrefixed :: Parser Datum
parseHashPrefixed =
    do
        void $! single '#'
        ( parseSeparated (single 'f')
            *> (pure $! Bool False)
            )
            <|> ( parseSeparated (single 't')
                    *> (pure $! Bool True)
                )
            <|> parseChar
            <* spaces

-- | Parse a character liteeral
parseChar :: Parser Datum
parseChar = do
    void $! single '\\'
    text <- takeWhile1P (Just "character name") (not . isSeparator)
    void $! spaces
    if
        | T.length text == 1 -> pure $! Char $! T.head text
        | text == "newline" -> pure $! Char '\n'
        | otherwise -> fail $ "Unsupported character literal: #\\" <> toString text

-- | Parse a string
parseString :: Parser Datum
parseString = label "string" do
    void $! single '"'
    texts <- many parseStringElement
    single '"' *> spaces
    pure $! String $! T.concat texts

-- | Parse a string element
parseStringElement :: Parser Text
parseStringElement = literalText <|> escapeSequence
  where
    literalText :: Parser Text
    literalText = takeWhile1P (Just "literal text") (\ch -> not $! ch `T.elem` "\"\\")

    escapeSequence :: Parser Text
    escapeSequence = label "escape sequence" do
        void $! single '\\'
        ch <- anySingle
        case M.lookup ch escapableCharsMap of
            Just replacement -> pure $! replacement
            Nothing -> fail $ "Unknown escape ssequence in string: \\" <> show ch

-- | A mapping from escapable characters to the values they represent
escapableCharsMap :: M.Map Char Text
escapableCharsMap =
    M.fromList
        [ ('n', "\n"),
          ('t', "\t")
        ]

-- | Scheme separator characters
separators :: S.Set Char
separators = S.fromList "\"'(),;`"

-- | Test whether a character is one of the separators
isSeparator :: Char -> Bool
isSeparator sep = sep <= ' ' || S.member sep separators

-- | Parse a separator and return it wrapped in Maybe. If the separator is
-- EOF, return Nothing.
parseSeparator :: Parser (Maybe Char)
parseSeparator = eof $> Nothing <|> fmap Just (satisfy isSeparator)

-- | Parse a token separated by a separator from the rest of the input. The separator is not consumed.
parseSeparated :: Parser a -> Parser a
parseSeparated p = p <* lookAhead parseSeparator

-- | Determine if a character is a horizontal (in-line) space
isHSpace :: Char -> Bool
isHSpace ch = ch == ' ' || ch == '\t'

-- | Determine if a character is a vertical space
isVSpace :: Char -> Bool
isVSpace ch = ch == '\n'

-- | Determine if a character is a space
isSpace :: Char -> Bool
isSpace ch = isHSpace ch || isVSpace ch

-- | Parse optional sequence of space
spaces :: Parser ()
spaces = void $! takeWhileP (Just "space") isSpace

-- | Parse a sequence of space. At least one space character needs to be found.
spaces1 :: Parser ()
spaces1 = void $! takeWhile1P (Just "space") isSpace
