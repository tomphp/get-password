module Main (main) where

import App (app)
import Args.CliArgs (CliArgsT, runCliArgsT)
import ConfigLoader.MacLoader (MacLoaderT, runMacLoaderT)
import Console.IOConsole (IOConsoleT, runIOConsoleT)
import LastPass.CliLastPass (CliLastPassT, runCliLastPassT)
import Printer.SimplePrinter (SimplePrinterT, runSimplePrinterT)

main :: IO ()
main = do
  runStack app

runStack :: CliLastPassT (SimplePrinterT (MacLoaderT (CliArgsT (IOConsoleT m)))) a -> m a
runStack =
  runIOConsoleT
    . runCliArgsT
    . runMacLoaderT
    . runSimplePrinterT
    . runCliLastPassT
