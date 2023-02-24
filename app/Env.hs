module Env (Env (..)) where

import Args.Class as Args
import ConfigLoader.Class as ConfigLoader
import LastPass.Class as LastPass
import Lens.Micro.TH (makeLenses)
import Printer.Class as Printer
import System.Class as System

data Env = Env
  { _args :: !Args,
    _configLoader :: !ConfigLoader,
    _lastPass :: !LastPass,
    _printer :: !Printer,
    _system :: !System
  }

makeLenses ''Env

instance HasArgs Env where
  args = Env.args

instance HasConfigLoader Env where
  configLoader = Env.configLoader

instance HasLastPass Env where
  lastPass = Env.lastPass

instance HasPrinter Env where
  printer = Env.printer

instance HasSystem Env where
  system = Env.system
