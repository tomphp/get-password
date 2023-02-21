module LastPass.EntryListParser (parse) where

import Control.Monad (void)
import qualified Data.Bifunctor as Bifunctor
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Void (Void)
import LastPass.Entry (Entry (Entry, id, name, url), EntryID (EntryID))
import Text.Megaparsec (MonadParsec (eof), Parsec, anySingle, between, many, manyTill, noneOf, some, try, (<?>), (<|>))
import qualified Text.Megaparsec
import Text.Megaparsec.Char (char, digitChar, eol, spaceChar)
import Text.Megaparsec.Error (errorBundlePretty)

parse :: Text -> Either String [Entry]
parse = Bifunctor.first errorBundlePretty . Text.Megaparsec.parse entries ""

entries :: Parsec Void Text [Entry]
entries = many entry

entry :: Parsec Void Text Entry
entry = do
  id' <- entryId
  void spaceChar
  name <- entryName
  void spaceChar
  url <- entryUrl
  return Entry {id = EntryID id', ..}

entryId :: Parsec Void Text Text
entryId = Text.pack <$> some digitChar <?> "Entry ID"

entryName :: Parsec Void Text Text
entryName = quotedString <?> "Name"

entryUrl :: Parsec Void Text Text
entryUrl = Text.pack <$> manyTill anySingle (void eol <|> eof) <?> "URL"

quotedString :: Parsec Void Text Text
quotedString =
  Text.pack
    <$> between
      (char '"')
      (char '"' <?> "closing double quote")
      (try $ many (noneOf ['"']))
