module PasswordEntry (PasswordEntry (..)) where

data PasswordEntry = PasswordEntry
  { entryId :: !String,
    name :: !String,
    url :: !String
  }
  deriving (Show, Eq)
