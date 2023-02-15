module EntryListParserSpec (spec) where

import EntryListParser (parseEntryList)
import PasswordEntry (PasswordEntry (PasswordEntry, id, name, url))
import Test.Hspec

spec :: Spec
spec = describe "parseEntryList" $ do
  it "returns an empty list for an empty string" $ do
    parseEntryList "" `shouldBe` Right []

  it "returns entries" $ do
    parseEntryList
      ( mconcat
          [ "11111 \"ebay\" http://www.ebay.com\n",
            "22222 \"amazon\" http://www.amazon.com"
          ]
      )
      `shouldBe` Right
        [ PasswordEntry {id = "11111", name = "ebay", url = "http://www.ebay.com"},
          PasswordEntry {id = "22222", name = "amazon", url = "http://www.amazon.com"}
        ]
