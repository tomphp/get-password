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

    it "returns true when the search string is contained in the name with a different case" $ do
      let entry = Entry {id = "11111", name = "this will MaTcH", url = "http://www.ebay.com"}
      matches "mAtCh" entry `shouldBe` True

    it "returns true when the search string exactly matches the ID" $ do
      let entry = Entry {id = "12345", name = "ebay", url = "http://www.ebay.com"}
      matches "12345" entry `shouldBe` True

    it "returns false when the search string partially matches the ID" $ do
      let entry = Entry {id = "12345", name = "ebay", url = "http://www.ebay.com"}
      matches "23" entry `shouldBe` False
