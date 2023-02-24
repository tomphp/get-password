module Main (main) where

import App (app)
import Args.CliArgs (cliArgs)
import ConfigLoader.MacLoader (macConfigLoader)
import Env (Env (..))
import LastPass.CliLastPass (cliLastPass)
import Printer.SimplePrinter (simplePrinter)
import RIO
import System.Cli (cliSystem)

main :: IO ()
main = do
  runRIO env (RIO app)

env :: Env
env =
  Env
    { args = cliArgs,
      configLoader = macConfigLoader,
      lastPass = cliLastPass,
      printer = simplePrinter,
      system = cliSystem
    }
