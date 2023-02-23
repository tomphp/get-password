module Main (main) where

import App (app)
import Args.CliArgs (cliArgs)
import ConfigLoader.MacLoader (macConfigLoader)
import Control.Monad.Reader (ReaderT, runReaderT)
import Env (Env (..))
import LastPass.CliLastPass (cliLastPass)
import Printer.SimplePrinter (simplePrinter)

main :: IO ()
main = do
  runApp app

env :: Env
env =
  Env
    { args = cliArgs,
      configLoader = macConfigLoader,
      lastPass = cliLastPass,
      printer = simplePrinter
    }

runApp :: ReaderT Env m a -> m a
runApp = flip runReaderT env
