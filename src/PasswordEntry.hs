module PasswordEntry (PasswordEntry (..)) where

import Data.Text (Text)

data PasswordEntry = PasswordEntry
  { entryId :: !Text,
    name :: !Text,
    url :: !Text
  }
  deriving (Show, Eq)
