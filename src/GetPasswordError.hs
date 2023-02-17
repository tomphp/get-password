module GetPasswordError (GetPasswordError (..)) where

import Entry (Entry)
import LastPassError (LastPassError)

data GetPasswordError
  = LastPassErrored LastPassError
  | PasswordNotFound
  | MultiplePasswordsFound [Entry]
  deriving (Show, Eq)
