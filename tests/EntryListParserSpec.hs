module EntryListParserSpec (spec) where

import Entry (Entry (Entry, id, name, url))
import EntryListParser (parseEntryList)
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
        [ Entry {id = "11111", name = "ebay", url = "http://www.ebay.com"},
          Entry {id = "22222", name = "amazon", url = "http://www.amazon.com"}
        ]
