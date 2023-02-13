module Args (getArgs) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Text as Text
import LastPass.Entry (Search (Search))
import qualified System.Environment as Env

getArgs :: MonadIO m => m (Maybe Search)
getArgs = parseArgs <$> liftIO Env.getArgs

parseArgs :: [String] -> Maybe Search
parseArgs [search] = Just $ Search $ Text.pack search
parseArgs _ = Nothing
