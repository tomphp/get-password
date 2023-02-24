module Args.CliArgsSpec (spec) where

import RIO
import Test.Hspec
import Args.Class (getSearch, Args, HasArgs (getArgs))
import System.Class (System (..), HasSystem (getSystem))
import Args.CliArgs (cliArgs)
import LastPass.Entry (Search(Search))
import Lens.Micro.TH (makeLenses, lensRules)

data TestEnv = TestEnv {
  args :: !Args,
  system :: !System
}

instance HasArgs TestEnv where
  getArgs = args

instance HasSystem TestEnv where
  getSystem = system

env :: TestEnv
env = TestEnv {
  args = cliArgs,
  system = System {
    getArgs_ = pure ["search-term"],
    getProgName_ = undefined,
    getHomeDirectory_ = undefined,
    printLine_ = undefined,
    printError_ = undefined,
    execInteractive_ = undefined,
    exec_ = undefined
  }
}

spec :: Spec
spec = describe "cliArgs" $ do
  it "returns the search tearm " $ do
    -- let env' = env & (getArgs . system) .~ pure ["search-term"]
    runRIO env (RIO getSearch) `shouldReturn` Right (Search "search-term")

