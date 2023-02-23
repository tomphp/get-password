module Main (main) where

import App (app)
import Args.Class (Args, HasArgs (getArgs))
import Args.CliArgs (cliArgs)
import ConfigLoader.Class (ConfigLoader, HasConfigLoader (getConfigLoader))
import ConfigLoader.MacLoader (macConfigLoader)
import Control.Monad.Reader (ReaderT, runReaderT)
import LastPass.Class (HasLastPass (getLastPass), LastPass)
import LastPass.CliLastPass (cliLastPass)
import Printer.Class (HasPrinter (getPrinter), Printer)
import Printer.SimplePrinter (simplePrinter)

main :: IO ()
main = do
  runApp app

data Env = Env
  { args :: !Args,
    configLoader :: !ConfigLoader,
    lastPass :: !LastPass,
    printer :: !Printer
  }

instance HasArgs Env where
  getArgs = args

instance HasConfigLoader Env where
  getConfigLoader = configLoader

instance HasLastPass Env where
  getLastPass = lastPass

instance HasPrinter Env where
  getPrinter = printer

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
