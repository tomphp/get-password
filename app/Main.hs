module Main (main) where

import App (app)
import ConfigLoader.MacLoader (MacLoaderT (runMacLoaderT))
import Console.IOConsole (IOConsoleT (runIOConsoleT))
import LastPass.CliLastPass (CliLastPassT, runCliLastPassT)
import Printer.SimplePrinter (SimplePrinterT, runSimplePrinterT)

main :: IO ()
main = do
  runStack app

runStack :: CliLastPassT (SimplePrinterT (MacLoaderT (IOConsoleT m))) a -> m a
runStack = runIOConsoleT . runMacLoaderT . runSimplePrinterT . runCliLastPassT
