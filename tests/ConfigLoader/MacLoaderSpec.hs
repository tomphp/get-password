module ConfigLoader.MacLoaderSpec (spec) where

import ConfigLoader.Class (LoadConfigError (LoadConfigError))
import ConfigLoader.Config (Config (Config, user))
import ConfigLoader.MacLoader (ReadConfigError (ConfigFileDoesNotExist, ConfigFileParseError), defaultIfDoesNotExist, getConfigPath, readConfig)
import LastPass.Class (User (User))
import RIO
import RIO.ByteString (writeFile)
import System.Environment as Env
import Test.Hspec

withEnv :: String -> String -> IO () -> IO ()
withEnv name value action = do
  oldValue <- Env.getEnv name
  bracket_ (Env.setEnv name value) (Env.setEnv name oldValue) action

spec :: Spec
spec = describe "Config" $ do
  around_ (withEnv "HOME" "/home/runner") $
    describe "getConfigPath" $ do
      it "returns the config path" $ do
        getConfigPath `shouldReturn` "/home/runner/.config/get-password/config.yml"

  describe "readConfig" $ do
    it "errors when file does not exist" $ do
      pendingWith "Fixme"
    -- catch @IOError (removeFile "test-config.yml") (\_ -> return ())
    -- readConfig "test-config.yml" `shouldReturn` Left ConfigFileDoesNotExist

    it "errors when file does not contain valid YAML" $ do
      writeFile "test-config.yml" "invalid yaml"
      readConfig "test-config.yml"
        `shouldReturn` Left
          (ConfigFileParseError "Aeson exception:\nError in $: parsing Config failed, expected Object, but encountered String")

    it "returns Config when file is empty" $ do
      writeFile "test-config.yml" "user: \"user@example.com\""
      readConfig "test-config.yml"
        `shouldReturn` Right (Config {user = Just $ User "user@example.com"})

    it "returns Config when file does not contina user" $ do
      writeFile "test-config.yml" "unknown: \"ignore me\""
      readConfig "test-config.yml"
        `shouldReturn` Right (Config {user = Nothing})

  describe "defaultIfDoesNotExist" $ do
    let defaultConfig = Config {user = Just $ User "default@example.com"}

    it "returns the default config when the config file does not exist" $ do
      defaultIfDoesNotExist defaultConfig (Left ConfigFileDoesNotExist) `shouldBe` Right defaultConfig

    it "returns the config when the config file exists and is valid" $ do
      let testConfig = Config {user = Just $ User "test@example.com"}
      defaultIfDoesNotExist defaultConfig (Right testConfig) `shouldBe` Right testConfig

    it "returns the error when the config file exists and there is an error" $ do
      defaultIfDoesNotExist defaultConfig (Left (ConfigFileParseError "error")) `shouldBe` Left (LoadConfigError "error")
