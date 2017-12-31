module Specs.FastaSpec where

import Test.Hspec
import RosalindUtils

spec :: Spec
spec =
  describe "FASTA" $ do
    it "should return [] for empty input" $
      readFasta "" `shouldBe` []
    it "should create header for single line input" $
      readFasta ">header first" `shouldBe` [("header first", "")]
    it "should create value for key and concat lines split by \\n" $
      readFasta ">header first\nsecond line\nthird line" `shouldBe` [("header first", "second linethird line")]
    it "should create value for key and concat lines split by \\r\\n or \\n" $
      readFasta ">header first\r\nsecond line\nthird line" `shouldBe` [("header first", "second linethird line")]
    it "should create multiple keys ans corresponding values" $ do
      let input = ">header first\n\
                  \second line\n\
                  \third line\n\
                  \>header second\n\
                  \value second"
      readFasta input `shouldBe` [("header first", "second linethird line"),
                                  ("header second","value second")]
