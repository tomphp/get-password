module EntrySpec (spec) where

import Entry (Entry (Entry, id, name, url), matches)
import Test.Hspec

spec :: Spec
spec = describe "Entry" $ do
  describe "matches" $ do
    it "returns false when the search string does not match any fields" $ do
      let entry = Entry {id = "11111", name = "ebay", url = "http://www.ebay.com"}
      matches "nothing" entry `shouldBe` False

    it "returns true when the search string is contained in the name" $ do
      let entry = Entry {id = "11111", name = "this will match", url = "http://www.ebay.com"}
      matches "will" entry `shouldBe` True

    it "returns true when the search string is contained in the name" $ do
      let entry = Entry {id = "11111", name = "", url = "http://www.ebay.com"}
      matches "ebay" entry `shouldBe` True

  it "is a test" $ do
    True `shouldBe` True
