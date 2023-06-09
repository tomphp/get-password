module Args.CliArgsSpec (spec) where

import Args.Class (Args, GetArgsError (GetArgsError), HasArgs (args), getAction_, getSearch_)
import Args.CliArgs (cliArgs)
import GetPassword (GetAction (CopyPassword, ShowPassword))
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

setArgs :: [Text] -> Env -> Env
setArgs newArgs oldEnv = oldEnv {_system = (_system env) {_getArgs = pure newArgs}}

spec :: Spec
spec = describe "cliArgs" $ do
  describe "getSearch" $ do
    it "returns an error when search term is missing" $ do
      let env' = setArgs [] env
      runRIO env' (RIO getSearch_) `shouldReturn` Left (GetArgsError "get-password-test")

    it "returns an error when search term is missing and show flag is provided" $ do
      let env' = setArgs ["--show"] env
      runRIO env' (RIO getSearch_) `shouldReturn` Left (GetArgsError "get-password-test")

    it "returns the search term" $ do
      let env' = setArgs ["search-term"] env
      runRIO env' (RIO getSearch_) `shouldReturn` Right (Search "search-term")

  describe "getAction" $ do
    it "returns an error when search term is missing" $ do
      let env' = setArgs [] env
      runRIO env' (RIO getAction_) `shouldReturn` Left (GetArgsError "get-password-test")

    it "returns an error when search term is missing and show flag is provided" $ do
      let env' = setArgs ["--show"] env
      runRIO env' (RIO getAction_) `shouldReturn` Left (GetArgsError "get-password-test")

    it "returns copy when not flag is provided" $ do
      let env' = setArgs ["search-term"] env
      runRIO env' (RIO getAction_) `shouldReturn` Right CopyPassword

    it "returns shwn when flag is provided" $ do
      let env' = setArgs ["--show", "search-term"] env
      runRIO env' (RIO getAction_) `shouldReturn` Right ShowPassword
