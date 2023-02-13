module Console.IOConsole
  ( IOConsoleT,
    runIOConsoleT,
  )
where

import Console.Class (MonadConsole (..))
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Text.IO as TextIO
import System.IO (stderr)

newtype IOConsoleT m a = IOConsoleT {runIOConsoleT :: m a}
  deriving stock (Functor)
  deriving newtype (Applicative, Monad, MonadIO)

instance MonadIO m => MonadConsole (IOConsoleT m) where
  printLine = liftIO . TextIO.putStrLn
  printErrorLine = liftIO . TextIO.hPutStrLn stderr
