module LastPass.Error (LastPassError (..)) where

import RIO

data LastPassError
  = NotInstalled
  | LoginFailed
  | ListPasswordsFailed
  | ListPasswordsParseFailed !String
  | ShowPasswordFailed !String
  | CopyPasswordFailed !String
  deriving stock (Show, Eq)
