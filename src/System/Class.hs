module System.Class (MonadSystem (..), System (..), HasSystem (..)) where

import Control.Monad.Except (ExceptT)
import RIO

class MonadIO m => MonadSystem m where
  getArgs :: m [Text]
  getProgName :: m Text
  getHomeDirectory :: m FilePath
  printLine :: Text -> m ()
  printError :: Text -> m ()
  execInteractive :: Text -> m ExitCode
  exec :: FilePath -> [Text] -> m (Either () Text)

data System = System
  { getArgs_ :: !(forall m. MonadIO m => m [Text]),
    getProgName_ :: !(forall m. MonadIO m => m Text),
    getHomeDirectory_ :: !(forall m. MonadIO m => m FilePath),
    printLine_ :: !(forall m. MonadIO m => Text -> m ()),
    printError_ :: !(forall m. MonadIO m => Text -> m ()),
    execInteractive_ :: !(forall m. MonadIO m => Text -> m ExitCode),
    exec_ :: !(forall m. (MonadIO m) => FilePath -> [Text] -> m (Either () Text))
  }

class HasSystem env where
  getSystem :: env -> System

instance HasSystem System where
  getSystem = id

instance (MonadIO m, HasSystem env) => MonadSystem (ReaderT env m) where
  getArgs = do
    env <- ask
    liftIO $ getArgs_ (getSystem env)
  getProgName = do
    env <- ask
    liftIO $ getProgName_ (getSystem env)
  getHomeDirectory = do
    env <- ask
    liftIO $ getHomeDirectory_ (getSystem env)
  printLine line = do
    env <- ask
    liftIO $ printLine_ (getSystem env) line
  printError line = do
    env <- ask
    liftIO $ printError_ (getSystem env) line
  execInteractive program = do
    env <- ask
    liftIO $ execInteractive_ (getSystem env) program
  exec program args = do
    env <- ask
    liftIO $ exec_ (getSystem env) program args

instance (MonadSystem m) => MonadSystem (ExceptT env m) where
  getArgs = lift getArgs
  getProgName = lift getProgName
  getHomeDirectory = lift getHomeDirectory
  printLine = lift . printLine
  printError = lift . printError
  execInteractive = lift . execInteractive
  exec program = lift . exec program
