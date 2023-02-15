module Console.IOConsole
  ( IOConsoleT,
    runIOConsoleT,
    IOConsole,
    runIOConsole,
  )
where

import Console.Class (MonadConsole (..))
import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.Text.IO as TextIO
import System.IO (stderr)

type IOConsole = IOConsoleT IO

runIOConsole :: IOConsole a -> IO a
runIOConsole = runIOConsoleT

newtype IOConsoleT m a = IOConsoleT {runIOConsoleT :: m a}
  deriving stock (Functor)
  deriving newtype (Applicative, Monad, MonadIO)

instance MonadIO m => MonadConsole (IOConsoleT m) where
  printLine = liftIO . TextIO.putStrLn
  printErrorLine = liftIO . TextIO.hPutStrLn stderr
