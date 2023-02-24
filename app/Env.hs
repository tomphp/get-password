module Env (Env (..)) where

import Args.Class (Args, HasArgs (getArgs))
import ConfigLoader.Class (ConfigLoader, HasConfigLoader (getConfigLoader))
import LastPass.Class (HasLastPass (getLastPass), LastPass)
import Printer.Class (HasPrinter (getPrinter), Printer)
import System.Class (HasSystem (getSystem), System)

data Env = Env
  { args :: !Args,
    configLoader :: !ConfigLoader,
    lastPass :: !LastPass,
    printer :: !Printer,
    system :: !System
  }

instance HasArgs Env where
  getArgs = args

instance HasConfigLoader Env where
  getConfigLoader = configLoader

instance HasLastPass Env where
  getLastPass = lastPass

instance HasPrinter Env where
  getPrinter = printer

instance HasSystem Env where
  getSystem = system
