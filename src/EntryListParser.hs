module EntryListParser (parseEntryList) where

import Control.Monad (void)
import Data.Bifunctor (Bifunctor (first))
import Data.Text (Text, pack)
import Data.Void (Void)
import PasswordEntry (PasswordEntry (PasswordEntry, entryId, name, url))
import Text.Megaparsec (MonadParsec (eof), Parsec, anySingle, between, many, manyTill, noneOf, parse, some, try, (<?>), (<|>))
import Text.Megaparsec.Char (char, digitChar, eol, spaceChar)
import Text.Megaparsec.Error (errorBundlePretty)

parseEntryList :: Text -> Either String [PasswordEntry]
parseEntryList = first errorBundlePretty . parse parseEntries ""

parseEntries :: Parsec Void Text [PasswordEntry]
parseEntries = many parseEntry

parseEntry :: Parsec Void Text PasswordEntry
parseEntry = do
  entryId <- pack <$> some digitChar <?> "Entry ID"
  void spaceChar
  name <- pack <$> between (char '"') (char '"' <?> "closing double quote") (try $ many (noneOf ['"'])) <?> "Name"
  void spaceChar
  url <- pack <$> manyTill anySingle (void eol <|> eof) <?> "URL"
  return PasswordEntry {..}
