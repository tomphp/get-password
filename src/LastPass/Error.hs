module LastPass.Error (LastPassError (..)) where

data LastPassError
  = NotInstalled
  | LoginFailed
  | ListPasswordsFailed
  | ListPasswordsParseFailed String
  | ShowPasswordFailed String
  deriving (Show, Eq)
