module Args (getArgs) where

import Console.Class (MonadConsole, printErrorLine)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text)
import qualified Data.Text as Text
import LastPass.Entry (Search (Search))
import qualified System.Environment as Env
import System.Exit (exitFailure)

getArgs :: (MonadConsole m, MonadIO m) => m Search
getArgs = do
  args <- parseArgs <$> liftIO Env.getArgs
  case args of
    Just search -> pure search
    Nothing -> getProgName >>= printUsage >> liftIO exitFailure

parseArgs :: [String] -> Maybe Search
parseArgs [search] = Just $ Search $ Text.pack search
parseArgs _ = Nothing

printUsage :: (MonadConsole m) => Text -> m ()
printUsage progName = printErrorLine ("Usage: " <> progName <> " <search>")

getProgName :: (MonadConsole m, MonadIO m) => m Text
getProgName = Text.pack <$> liftIO Env.getProgName
