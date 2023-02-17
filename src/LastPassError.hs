module LastPassError (LastPassError (..)) where

import Entry (Entry)

data LastPassError
  = LastPassNotInstalled
  | LastPassNotLoggedIn
  | LastPassListPasswordsFailed
  | LastPassListPasswordsParseFailed String
  | LastPassShowPasswordFailed String
  | LastPassPasswordNotFound
  | LastPassMultiplePasswordsFound [Entry]
  deriving (Show, Eq)
