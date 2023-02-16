module GetPasswordSpec (spec) where

import Entry (Entry (Entry, id, name, url))
import GetPassword (getPassword)
import LastPass (LastPassError (..))
import LastPassMock
  ( checkIsInstalledWillReturn,
    checkIsLoggedInWillReturn,
    listPasswordsWillReturn,
    runMock,
    showPasswordWillReturn,
  )
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "getPasswords" $ do
    it "errors when lastpass is not installed" $ do
      let (result, history) = runMock $ do
            checkIsInstalledWillReturn (Left LastPassNotInstalled)
            getPassword "search-string"
      result `shouldBe` Left LastPassNotInstalled
      history `shouldBe` ["checkIsInstalled"]

    it "errors when lastpass is not logged in" $ do
      let (result, history) = runMock $ do
            checkIsLoggedInWillReturn $ Left LastPassNotLoggedIn
            getPassword "search-string"
      result `shouldBe` Left LastPassNotLoggedIn
      history
        `shouldBe` [ "checkIsInstalled",
                     "checkIsLoggedIn"
                   ]

    it "errors when list passwords fails" $ do
      let (result, history) = runMock $ do
            listPasswordsWillReturn (Left LastPassListPasswordsFailed)
            getPassword "search-string"
      result `shouldBe` Left LastPassListPasswordsFailed
      history
        `shouldBe` [ "checkIsInstalled",
                     "checkIsLoggedIn",
                     "listPasswords"
                   ]

    it "errors when not matches are found" $ do
      let (result, history) = runMock $ do
            listPasswordsWillReturn (Left LastPassListPasswordsFailed)
            getPassword "search-string"
      result `shouldBe` Left LastPassListPasswordsFailed
      history
        `shouldBe` [ "checkIsInstalled",
                     "checkIsLoggedIn",
                     "listPasswords"
                   ]

    it "errors when show password fails" $ do
      let (result, history) = runMock $ do
            listPasswordsWillReturn
              ( Right
                  [ Entry {id = "entry-id", name = "contains search", url = "url"}
                  ]
              )
            showPasswordWillReturn (Left $ LastPassShowPasswordFailed "reason")
            getPassword "search"
      result `shouldBe` Left (LastPassShowPasswordFailed "reason")
      history
        `shouldBe` [ "checkIsInstalled",
                     "checkIsLoggedIn",
                     "listPasswords",
                     "showPassword \"entry-id\""
                   ]

    it "errors when no matches are found" $ do
      let (result, history) = runMock $ do
            listPasswordsWillReturn
              ( Right
                  [ Entry {id = "entry-id", name = "does-not-match", url = "url"}
                  ]
              )
            showPasswordWillReturn (Left $ LastPassShowPasswordFailed "reason")
            getPassword "search"
      result `shouldBe` Left LastPassPasswordNotFound
      history
        `shouldBe` [ "checkIsInstalled",
                     "checkIsLoggedIn",
                     "listPasswords"
                   ]

    it "errors when multiple matches are found" $ do
      let (result, history) = runMock $ do
            listPasswordsWillReturn
              ( Right
                  [ Entry {id = "entry-id-1", name = "match one", url = "url1"},
                    Entry {id = "entry-id-2", name = "match two", url = "url2"}
                  ]
              )
            showPasswordWillReturn (Left $ LastPassShowPasswordFailed "reason")
            getPassword "match"
      result
        `shouldBe` Left
          ( LastPassMultiplePasswordsFound
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
        let (result, history) = runMock $ do
              showPasswordWillReturn (Right "secret")
              listPasswordsWillReturn
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
      let (result, history) = runMock $ do
            showPasswordWillReturn (Right "secret")
            listPasswordsWillReturn
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

    it "returns the password matching the url" $ do
      let (result, history) = runMock $ do
            showPasswordWillReturn (Right "secret")
            listPasswordsWillReturn
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
