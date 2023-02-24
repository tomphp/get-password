module GetPasswordSpec (spec) where

import GetPassword
  ( GetPasswordError (LastPassErrored, MultiplePasswordsFound, NotLoggedIn, PasswordNotFound),
    getPassword,
  )
import LastPass.Class (Password (Password), User (User))
import LastPass.Entry (Entry (Entry, id, name, url), EntryID (EntryID), Search (Search))
import LastPass.Error (LastPassError (ListPasswordsFailed, LoginFailed, NotInstalled, ShowPasswordFailed))
import qualified LastPassMock as Mock
import RIO
import Test.Hspec

spec :: Spec
spec = describe "GetPassword" $ do
  it "needs to be fixed" $
    pendingWith "FIXME"

--   describe "getPasswords" $ do
--     it "errors when lastpass is not installed" $ do
--       let (result, history) = Mock.run $ do
--             lift $ Mock.checkIsInstalledWillErrorWith NotInstalled
--             getPassword Nothing (Search "search-string")
--       result `shouldBe` Left (LastPassErrored NotInstalled)
--       history `shouldBe` ["checkIsInstalled"]

--     it "errors when lastpass is not logged in and user is not provided" $ do
--       let (result, history) = Mock.run $ do
--             lift $ Mock.isLoggedInWillReturn False
--             getPassword Nothing (Search "search-string")
--       result `shouldBe` Left NotLoggedIn
--       history
--         `shouldBe` [ "checkIsInstalled",
--                      "isLoggedIn"
--                    ]

--     it "errors when not logged in and login fails" $ do
--       let (result, history) = Mock.run $ do
--             lift $ Mock.isLoggedInWillReturn False
--             lift $ Mock.loginWillErrorWith LoginFailed
--             getPassword (Just $ User "user@example.com") (Search "search-string")
--       result `shouldBe` Left (LastPassErrored LoginFailed)
--       history
--         `shouldBe` [ "checkIsInstalled",
--                      "isLoggedIn",
--                      "login \"user@example.com\""
--                    ]

--     it "errors when list passwords fails" $ do
--       let (result, history) = Mock.run $ do
--             lift $ Mock.listPasswordsWillErrorWith ListPasswordsFailed
--             getPassword Nothing (Search "search-string")
--       result `shouldBe` Left (LastPassErrored ListPasswordsFailed)
--       history
--         `shouldBe` [ "checkIsInstalled",
--                      "isLoggedIn",
--                      "listPasswords"
--                    ]

--     it "errors when not matches are found" $ do
--       let (result, history) = Mock.run $ do
--             lift $ Mock.listPasswordsWillErrorWith ListPasswordsFailed
--             getPassword Nothing (Search "search-string")
--       result `shouldBe` Left (LastPassErrored ListPasswordsFailed)
--       history
--         `shouldBe` [ "checkIsInstalled",
--                      "isLoggedIn",
--                      "listPasswords"
--                    ]

--     it "errors when show password fails" $ do
--       let (result, history) = Mock.run $ do
--             lift $
--               Mock.listPasswordsWillReturn
--                 [ Entry {id = EntryID "entry-id", name = "contains search", url = "url"}
--                 ]
--             lift $ Mock.showPasswordWillErrorWith (ShowPasswordFailed "reason")
--             getPassword Nothing (Search "search")
--       result `shouldBe` Left (LastPassErrored $ ShowPasswordFailed "reason")
--       history
--         `shouldBe` [ "checkIsInstalled",
--                      "isLoggedIn",
--                      "listPasswords",
--                      "showPassword \"entry-id\""
--                    ]

--     it "errors when no matches are found" $ do
--       let (result, history) = Mock.run $ do
--             lift $
--               Mock.listPasswordsWillReturn
--                 [ Entry {id = EntryID "entry-id", name = "does-not-match", url = "url"}
--                 ]
--             lift $ Mock.showPasswordWillErrorWith (ShowPasswordFailed "reason")
--             getPassword Nothing (Search "search")
--       result `shouldBe` Left PasswordNotFound
--       history
--         `shouldBe` [ "checkIsInstalled",
--                      "isLoggedIn",
--                      "listPasswords"
--                    ]

--     it "errors when multiple matches are found" $ do
--       let (result, history) = Mock.run $ do
--             lift $
--               Mock.listPasswordsWillReturn
--                 [ Entry {id = EntryID "entry-id-1", name = "match one", url = "url1"},
--                   Entry {id = EntryID "entry-id-2", name = "match two", url = "url2"}
--                 ]
--             lift $ Mock.showPasswordWillErrorWith (ShowPasswordFailed "reason")
--             getPassword Nothing (Search "match")
--       result
--         `shouldBe` Left
--           ( MultiplePasswordsFound
--               [ Entry {id = EntryID "entry-id-1", name = "match one", url = "url1"},
--                 Entry {id = EntryID "entry-id-2", name = "match two", url = "url2"}
--               ]
--           )
--       history
--         `shouldBe` [ "checkIsInstalled",
--                      "isLoggedIn",
--                      "listPasswords"
--                    ]

--     it "returns the password after logging in" $
--       do
--         let (result, history) = Mock.run $ do
--               lift $ Mock.isLoggedInWillReturn False
--               lift $ Mock.showPasswordWillReturn (Password "secret")
--               lift $
--                 Mock.listPasswordsWillReturn
--                   [ Entry {id = EntryID "entry-id-1", name = "contains search", url = "url1"},
--                     Entry {id = EntryID "entry-id-2", name = "other", url = "url2"}
--                   ]
--               getPassword (Just $ User "user@example.com") (Search "search")
--         result `shouldBe` Right (Password "secret")
--         history
--           `shouldBe` [ "checkIsInstalled",
--                        "isLoggedIn",
--                        "login \"user@example.com\"",
--                        "listPasswords",
--                        "showPassword \"entry-id-1\""
--                      ]

--     it "returns the password matching the name" $
--       do
--         let (result, history) = Mock.run $ do
--               lift $ Mock.showPasswordWillReturn (Password "secret")
--               lift $
--                 Mock.listPasswordsWillReturn
--                   [ Entry {id = EntryID "entry-id-1", name = "contains search", url = "url1"},
--                     Entry {id = EntryID "entry-id-2", name = "other", url = "url2"}
--                   ]
--               getPassword Nothing (Search "search")
--         result `shouldBe` Right (Password "secret")
--         history
--           `shouldBe` [ "checkIsInstalled",
--                        "isLoggedIn",
--                        "listPasswords",
--                        "showPassword \"entry-id-1\""
--                      ]

--     it "returns the password matching the url" $ do
--       let (result, history) = Mock.run $ do
--             lift $ Mock.showPasswordWillReturn (Password "secret")
--             lift $
--               Mock.listPasswordsWillReturn
--                 [ Entry {id = EntryID "entry-id-1", name = "does-not-match", url = "url1"},
--                   Entry {id = EntryID "entry-id-2", name = "matches", url = "http://example.com"}
--                 ]
--             getPassword Nothing (Search "example")
--       result `shouldBe` Right (Password "secret")
--       history
--         `shouldBe` [ "checkIsInstalled",
--                      "isLoggedIn",
--                      "listPasswords",
--                      "showPassword \"entry-id-2\""
--                    ]
