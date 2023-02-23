module LastPass.CliLastPass (cliLastPass) where

import LastPass.Class (LastPass (LastPass, checkIsInstalled_, isLoggedIn_, listPasswords_, login_, showPassword_))
import qualified LastPass.Cli as Cli

cliLastPass :: LastPass
cliLastPass =
  LastPass
    { checkIsInstalled_ = Cli.checkIsInstalled,
      isLoggedIn_ = Cli.isLoggedIn,
      login_ = Cli.login,
      listPasswords_ = Cli.listPasswords,
      showPassword_ = Cli.showPassword
    }
