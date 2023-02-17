module GetPasswordSpec (spec) where

import Control.Monad.RWS (lift)
import GetPassword
  ( GetPasswordError (LastPassErrored, MultiplePasswordsFound, PasswordNotFound),
    getPassword,
  )
import LastPass
  ( Entry (Entry, id, name, url),
    LastPassError (ListPasswordsFailed, NotInstalled, NotLoggedIn, ShowPasswordFailed),
  )
import qualified LastPassMock as Mock
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "getPasswords" $ do
    it "errors when lastpass is not installed" $ do
      let (result, history) = Mock.run $ do
            lift $ Mock.checkIsInstalledWillErrorWith NotInstalled
            getPassword "search-string"
      result `shouldBe` Left (LastPassErrored NotInstalled)
      history `shouldBe` ["checkIsInstalled"]

    it "errors when lastpass is not logged in" $ do
      let (result, history) = Mock.run $ do
            lift $ Mock.checkIsLoggedInWillErrorWith NotLoggedIn
            getPassword "search-string"
      result `shouldBe` Left (LastPassErrored NotLoggedIn)
      history
        `shouldBe` [ "checkIsInstalled",
                     "checkIsLoggedIn"
                   ]

    it "errors when list passwords fails" $ do
      let (result, history) = Mock.run $ do
            lift $ Mock.listPasswordsWillErrorWith ListPasswordsFailed
            getPassword "search-string"
      result `shouldBe` Left (LastPassErrored ListPasswordsFailed)
      history
        `shouldBe` [ "checkIsInstalled",
                     "checkIsLoggedIn",
                     "listPasswords"
                   ]

    it "errors when not matches are found" $ do
      let (result, history) = Mock.run $ do
            lift $ Mock.listPasswordsWillErrorWith ListPasswordsFailed
            getPassword "search-string"
      result `shouldBe` Left (LastPassErrored ListPasswordsFailed)
      history
        `shouldBe` [ "checkIsInstalled",
                     "checkIsLoggedIn",
                     "listPasswords"
                   ]

    it "errors when show password fails" $ do
      let (result, history) = Mock.run $ do
            lift $
              Mock.listPasswordsWillReturn
                [ Entry {id = "entry-id", name = "contains search", url = "url"}
                ]
            lift $ Mock.showPasswordWillErrorWith (ShowPasswordFailed "reason")
            getPassword "search"
      result `shouldBe` Left (LastPassErrored $ ShowPasswordFailed "reason")
      history
        `shouldBe` [ "checkIsInstalled",
                     "checkIsLoggedIn",
                     "listPasswords",
                     "showPassword \"entry-id\""
                   ]

    it "errors when no matches are found" $ do
      let (result, history) = Mock.run $ do
            lift $
              Mock.listPasswordsWillReturn
                [ Entry {id = "entry-id", name = "does-not-match", url = "url"}
                ]
            lift $ Mock.showPasswordWillErrorWith (ShowPasswordFailed "reason")
            getPassword "search"
      result `shouldBe` Left PasswordNotFound
      history
        `shouldBe` [ "checkIsInstalled",
                     "checkIsLoggedIn",
                     "listPasswords"
                   ]

    it "errors when multiple matches are found" $ do
      let (result, history) = Mock.run $ do
            lift $
              Mock.listPasswordsWillReturn
                [ Entry {id = "entry-id-1", name = "match one", url = "url1"},
                  Entry {id = "entry-id-2", name = "match two", url = "url2"}
                ]
            lift $ Mock.showPasswordWillErrorWith (ShowPasswordFailed "reason")
            getPassword "match"
      result
        `shouldBe` Left
          ( MultiplePasswordsFound
              [ Entry {id = "entry-id-1", name = "match one", url = "url1"},
                Entry {id = "entry-id-2", name = "match two", url = "url2"}
              ]
          )
      history
        `shouldBe` [ "checkIsInstalled",
                     "checkIsLoggedIn",
                     "listPasswords"
                   ]

    it "returns the password matching the name" $
      do
        let (result, history) = Mock.run $ do
              lift $ Mock.showPasswordWillReturn "secret"
              lift $
                Mock.listPasswordsWillReturn
                  [ Entry {id = "entry-id-1", name = "contains search", url = "url1"},
                    Entry {id = "entry-id-2", name = "other", url = "url2"}
                  ]
              getPassword "search"
        result `shouldBe` Right "secret"
        history
          `shouldBe` [ "checkIsInstalled",
                       "checkIsLoggedIn",
                       "listPasswords",
                       "showPassword \"entry-id-1\""
                     ]

    it "returns the password matching the url" $ do
      let (result, history) = Mock.run $ do
            lift $ Mock.showPasswordWillReturn "secret"
            lift $
              Mock.listPasswordsWillReturn
                [ Entry {id = "entry-id-1", name = "does-not-match", url = "url1"},
                  Entry {id = "entry-id-2", name = "matches", url = "http://example.com"}
                ]
            getPassword "example"
      result `shouldBe` Right "secret"
      history
        `shouldBe` [ "checkIsInstalled",
                     "checkIsLoggedIn",
                     "listPasswords",
                     "showPassword \"entry-id-2\""
                   ]
