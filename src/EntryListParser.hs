module EntryListParser (parseEntryList) where

import Control.Monad (void)
import Data.Bifunctor (Bifunctor (first))
import Data.Text (Text, pack)
import Data.Void (Void)
import PasswordEntry (PasswordEntry (PasswordEntry, id, name, url))
import Text.Megaparsec (MonadParsec (eof), Parsec, anySingle, between, many, manyTill, noneOf, parse, some, try, (<?>), (<|>))
import Text.Megaparsec.Char (char, digitChar, eol, spaceChar)
import Text.Megaparsec.Error (errorBundlePretty)

parseEntryList :: Text -> Either String [PasswordEntry]
parseEntryList = first errorBundlePretty . parse entries ""

entries :: Parsec Void Text [PasswordEntry]
entries = many entry

entry :: Parsec Void Text PasswordEntry
entry = do
  id' <- entryId
  void spaceChar
  name <- entryName
  void spaceChar
  url <- entryUrl
  return PasswordEntry {id = id', ..}

entryId :: Parsec Void Text Text
entryId = pack <$> some digitChar <?> "Entry ID"

entryName :: Parsec Void Text Text
entryName = pack <$> between (char '"') (char '"' <?> "closing double quote") (try $ many (noneOf ['"'])) <?> "Name"

entryUrl :: Parsec Void Text Text
entryUrl = pack <$> manyTill anySingle (void eol <|> eof) <?> "URL"
