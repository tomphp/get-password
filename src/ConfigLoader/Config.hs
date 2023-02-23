module ConfigLoader.Config (Config (..), defaultConfig) where

import Data.Yaml (FromJSON (parseJSON), withObject, (.!=), (.:?))
import LastPass.Class (User)
import RIO

newtype Config = Config
  { user :: Maybe User
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON Config where
  parseJSON = withObject "Config" $ \obj ->
    Config
      <$> obj .:? "user" .!= Nothing

defaultConfig :: Config
defaultConfig = Config {user = Nothing}
