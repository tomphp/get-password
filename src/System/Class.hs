module System.Class (System (..), HasSystem (..), getArgs_, getProgName_, getHomeDirectory_, printLine_, printError_, execInteractive_, exec_) where

import Lens.Micro.TH (makeClassy)
import RIO

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

getArgs_ :: (MonadReader env m, MonadIO m, HasSystem env) => m [Text]
getArgs_ = ask >>= view getArgs

getProgName_ :: (MonadReader env m, MonadIO m, HasSystem env) => m Text
getProgName_ = ask >>= view getProgName

getHomeDirectory_ :: (MonadReader env m, MonadIO m, HasSystem env) => m FilePath
getHomeDirectory_ = ask >>= view getHomeDirectory

printLine_ :: (MonadReader env m, MonadIO m, HasSystem env) => Text -> m ()
printLine_ line = ask >>= view printLine <*> pure line

printError_ :: (MonadReader env m, MonadIO m, HasSystem env) => Text -> m ()
printError_ line = ask >>= view printError <*> pure line

execInteractive_ :: (MonadReader env m, MonadIO m, HasSystem env) => Text -> m ExitCode
execInteractive_ program = ask >>= view execInteractive <*> pure program

exec_ :: (MonadReader env m, MonadIO m, HasSystem env) => FilePath -> [Text] -> m (Either () Text)
exec_ program args = ask >>= view exec <*> pure program <*> pure args
