module GetPasswordSpec (spec) where

import Control.Monad.RWS (lift)
import Entry (Entry (Entry, id, name, url))
import GetPassword (getPassword)
import GetPasswordError (GetPasswordError (LastPassErrored, MultiplePasswordsFound, PasswordNotFound))
import LastPassError (LastPassError (NotInstalled, NotLoggedIn, ListPasswordsFailed, ShowPasswordFailed))
import LastPassMock as Mock
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "getPasswords" $ do
    it "errors when lastpass is not installed" $ do
      let (result, history) = Mock.run $ do
            lift $ Mock.checkIsInstalledWillReturn (Left NotInstalled)
            getPassword "search-string"
      result `shouldBe` Left (LastPassErrored NotInstalled)
      history `shouldBe` ["checkIsInstalled"]

    it "errors when lastpass is not logged in" $ do
      let (result, history) = Mock.run $ do
            lift $ Mock.checkIsLoggedInWillReturn $ Left NotLoggedIn
            getPassword "search-string"
      result `shouldBe` Left (LastPassErrored NotLoggedIn)
      history
        `shouldBe` [ "checkIsInstalled",
                     "checkIsLoggedIn"
                   ]

    it "errors when list passwords fails" $ do
      let (result, history) = Mock.run $ do
            lift $ Mock.listPasswordsWillReturn (Left ListPasswordsFailed)
            getPassword "search-string"
      result `shouldBe` Left (LastPassErrored ListPasswordsFailed)
      history
        `shouldBe` [ "checkIsInstalled",
                     "checkIsLoggedIn",
                     "listPasswords"
                   ]

    it "errors when not matches are found" $ do
      let (result, history) = Mock.run $ do
            lift $ Mock.listPasswordsWillReturn (Left ListPasswordsFailed)
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
                ( Right
                    [ Entry {id = "entry-id", name = "contains search", url = "url"}
                    ]
                )
            lift $ Mock.showPasswordWillReturn (Left $ ShowPasswordFailed "reason")
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
                ( Right
                    [ Entry {id = "entry-id", name = "does-not-match", url = "url"}
                    ]
                )
            lift $ Mock.showPasswordWillReturn (Left $ ShowPasswordFailed "reason")
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
                ( Right
                    [ Entry {id = "entry-id-1", name = "match one", url = "url1"},
                      Entry {id = "entry-id-2", name = "match two", url = "url2"}
                    ]
                )
            lift $ Mock.showPasswordWillReturn (Left $ ShowPasswordFailed "reason")
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
              lift $ Mock.showPasswordWillReturn (Right "secret")
              lift $
                Mock.listPasswordsWillReturn
                  ( Right
                      [ Entry {id = "entry-id-1", name = "contains search", url = "url1"},
                        Entry {id = "entry-id-2", name = "other", url = "url2"}
                      ]
                  )
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
            lift $ Mock.showPasswordWillReturn (Right "secret")
            lift $
              Mock.listPasswordsWillReturn
                ( Right
                    [ Entry {id = "entry-id-1", name = "does-not-match", url = "url1"},
                      Entry {id = "entry-id-2", name = "matches", url = "http://example.com"}
                    ]
                )
            getPassword "example"
      result `shouldBe` Right "secret"
      history
        `shouldBe` [ "checkIsInstalled",
                     "checkIsLoggedIn",
                     "listPasswords",
                     "showPassword \"entry-id-2\""
                   ]
