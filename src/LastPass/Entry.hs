module LastPass.Entry (Entry (..), EntryID (EntryID), Search (Search), matches) where

import RIO
import RIO.Text (isInfixOf)
import qualified RIO.Text as Text

newtype EntryID = EntryID Text
  deriving stock (Show, Eq)

data Entry = Entry
  { id :: !EntryID,
    name :: !Text,
    url :: !Text
  }
  deriving stock (Show, Eq)

newtype Search = Search Text
  deriving stock (Show, Eq)

matches :: Search -> Entry -> Bool
matches search entry
  | entryIDMatches search entryID = True
  | nameMatches search name = True
  | urlMatches search url = True
  | otherwise = False
  where
    Entry {id = entryID, name, url} = entry

entryIDMatches :: Search -> EntryID -> Bool
entryIDMatches (Search search) (EntryID entryID) = Text.toLower search == Text.toLower entryID

nameMatches :: Search -> Text -> Bool
nameMatches (Search search) name = Text.toLower search `isInfixOf` Text.toLower name

urlMatches :: Search -> Text -> Bool
urlMatches (Search search) url = Text.toLower search `isInfixOf` Text.toLower url
