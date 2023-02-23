module Env (Env (..)) where

import Args.Class (Args, HasArgs (getArgs))
import ConfigLoader.Class (ConfigLoader, HasConfigLoader (getConfigLoader))
import LastPass.Class (HasLastPass (getLastPass), LastPass)
import Printer.Class (HasPrinter (getPrinter), Printer)

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
