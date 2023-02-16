module Entry (Entry (..), matches) where

import Data.Text (Text, isInfixOf)

data Entry = Entry
  { id :: !Text,
    name :: !Text,
    url :: !Text
  }
  deriving (Show, Eq)

matches :: Text -> Entry -> Bool
matches search entry = search `isInfixOf` Entry.name entry || search `isInfixOf` Entry.url entry
