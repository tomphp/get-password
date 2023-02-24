module App.Error where

import ConfigLoader.Class (LoadConfigError)
import GetPassword (GetPasswordError)
import RIO

data AppError
  = AppGetArgsError !Text
  | AppLoadConfigError !LoadConfigError
  | AppGetPasswordError !GetPasswordError
