module System.Class (MonadSystem (..), System (..), HasSystem (..)) where

import Control.Monad.Except (ExceptT)
import Lens.Micro.TH (makeClassy)
import RIO

class MonadIO m => MonadSystem m where
  getArgs_ :: m [Text]
  getProgName_ :: m Text
  getHomeDirectory_ :: m FilePath
  printLine_ :: Text -> m ()
  printError_ :: Text -> m ()
  execInteractive_ :: Text -> m ExitCode
  exec_ :: FilePath -> [Text] -> m (Either () Text)

data System = System
  { _getArgs :: !(forall m. MonadIO m => m [Text]),
    _getProgName :: !(forall m. MonadIO m => m Text),
    _getHomeDirectory :: !(forall m. MonadIO m => m FilePath),
    _printLine :: !(forall m. MonadIO m => Text -> m ()),
    _printError :: !(forall m. MonadIO m => Text -> m ()),
    _execInteractive :: !(forall m. MonadIO m => Text -> m ExitCode),
    _exec :: !(forall m. (MonadIO m) => FilePath -> [Text] -> m (Either () Text))
  }

makeClassy ''System

instance (MonadIO m, HasSystem env) => MonadSystem (ReaderT env m) where
  getArgs_ = ask >>= liftIO <$> view getArgs
  getProgName_ = ask >>= liftIO <$> view getProgName
  getHomeDirectory_ = ask >>= liftIO <$> view getHomeDirectory
  printLine_ line = ask >>= liftIO <$> (view printLine <*> pure line)
  printError_ line = ask >>= liftIO <$> (view printError <*> pure line)
  execInteractive_ program = ask >>= liftIO <$> (view execInteractive <*> pure program)
  exec_ program args = ask >>= liftIO <$> (view exec <*> pure program <*> pure args)

instance (MonadSystem m) => MonadSystem (ExceptT env m) where
  getArgs_ = lift getArgs_
  getProgName_ = lift getProgName_
  getHomeDirectory_ = lift getHomeDirectory_
  printLine_ = lift . printLine_
  printError_ = lift . printError_
  execInteractive_ = lift . execInteractive_
  exec_ program = lift . exec_ program
