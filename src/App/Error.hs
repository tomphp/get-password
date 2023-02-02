module App.Error where

import ConfigLoader.Class (LoadConfigError)
import Data.Text (Text)
import GetPassword (GetPasswordError)

data AppError
  = AppGetArgsError Text
  | AppLoadConfigError LoadConfigError
  | AppGetPasswordError GetPasswordError
