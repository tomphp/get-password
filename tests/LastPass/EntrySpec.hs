module LastPass.EntrySpec (spec) where

import LastPass.Entry (Entry (Entry, id, name, url), EntryID (EntryID), Search (Search), matches)
import Test.Hspec

spec :: Spec
spec = describe "Entry" $ do
  describe "matches" $ do
    it "returns false when the search string does not match any fields" $ do
      let entry = Entry {id = EntryID "11111", name = "ebay", url = "http://www.ebay.com"}
      matches (Search "nothing") entry `shouldBe` False

    it "returns true when the search string is contained in the name" $ do
      let entry = Entry {id = EntryID "11111", name = "this will match", url = "http://www.ebay.com"}
      matches (Search "will") entry `shouldBe` True

    it "returns true when the search string is contained in the name" $ do
      let entry = Entry {id = EntryID "11111", name = "", url = "http://www.ebay.com"}
      matches (Search "ebay") entry `shouldBe` True

    it "returns true when the search string is contained in the name with a different case" $ do
      let entry = Entry {id = EntryID "11111", name = "this will MaTcH", url = "http://www.ebay.com"}
      matches (Search "mAtCh") entry `shouldBe` True

    it "returns true when the search string exactly matches the ID" $ do
      let entry = Entry {id = EntryID "12345", name = "ebay", url = "http://www.ebay.com"}
      matches (Search "12345") entry `shouldBe` True

    it "returns false when the search string partially matches the ID" $ do
      let entry = Entry {id = EntryID "12345", name = "ebay", url = "http://www.ebay.com"}
      matches (Search "23") entry `shouldBe` False
