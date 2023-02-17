module LastPass.Error (LastPassError (..)) where

data LastPassError
  = NotInstalled
  | NotLoggedIn
  | ListPasswordsFailed
  | ListPasswordsParseFailed String
  | ShowPasswordFailed String
  deriving (Show, Eq)
