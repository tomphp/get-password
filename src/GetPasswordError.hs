module GetPasswordError (GetPasswordError (..)) where

import LastPass.Entry (Entry)
import LastPass.Error (LastPassError)

data GetPasswordError
  = LastPassErrored LastPassError
  | PasswordNotFound
  | MultiplePasswordsFound [Entry]
  deriving (Show, Eq)
