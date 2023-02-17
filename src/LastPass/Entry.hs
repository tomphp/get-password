module LastPass.Entry (Entry (..), matches) where

import Data.Text (Text, isInfixOf)
import qualified Data.Text as Text

data Entry = Entry
  { id :: !Text,
    name :: !Text,
    url :: !Text
  }
  deriving (Show, Eq)

matches :: Text -> Entry -> Bool
matches search entry
  | lowerSearch == LastPass.Entry.id entry = True
  | lowerSearch `isInfixOf` lowerName = True
  | lowerSearch `isInfixOf` lowerUrl = True
  | otherwise = False
  where
    lowerSearch = Text.toLower search
    lowerName = Text.toLower (name entry)
    lowerUrl = Text.toLower (url entry)