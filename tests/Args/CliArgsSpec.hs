module Args.CliArgsSpec (spec) where

import Args.Class (Args, HasArgs (args), getSearch_)
import Args.CliArgs (cliArgs)
import LastPass.Entry (Search (Search))
import Lens.Micro.TH (makeLenses)
import RIO
import System.Class (HasSystem (system), System (..))
import Test.Hspec

data Env = Env
  { _args :: !Args,
    _system :: !System
  }

makeLenses ''Env

instance HasArgs Env where
  args = Args.CliArgsSpec.args

instance HasSystem Env where
  system = Args.CliArgsSpec.system

env :: Env
env =
  Env
    { _args = cliArgs,
      _system =
        System
          { _getArgs = pure ["search-term"],
            _getProgName = undefined,
            _getHomeDirectory = undefined,
            _printLine = undefined,
            _printError = undefined,
            _execInteractive = undefined,
            _exec = undefined
          }
    }

spec :: Spec
spec = describe "cliArgs" $ do
  it "returns the search tearm " $ do
    -- let env' = env & (getArgs . system) .~ pure ["search-term"]
    runRIO env (RIO getSearch_) `shouldReturn` Right (Search "search-term")
