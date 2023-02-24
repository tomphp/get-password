module LastPass.CliLastPass (cliLastPass) where

import LastPass.Class (LastPass (..))
import qualified LastPass.Cli as Cli

cliLastPass :: LastPass
cliLastPass =
  LastPass
    { _checkIsInstalled = Cli.checkIsInstalled,
      _isLoggedIn = Cli.isLoggedIn,
      _login = Cli.login,
      _listPasswords = Cli.listPasswords,
      _showPassword = Cli.showPassword
    }
