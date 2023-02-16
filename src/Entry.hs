module Entry (Entry (..)) where

import Data.Text (Text)

data Entry = Entry
  { id :: !Text,
    name :: !Text,
    url :: !Text
  }
  deriving (Show, Eq)
