module Specs.TestSpec where

import Test.Hspec
import RosalindUtils

spec :: Spec
spec =
  describe "utils" $
    describe "createRna" $
      it "should translate RNA to Protein" $ do
        let (Just rna) =
              createRna "AUGGCCAUGGCGCCCAGAACUGAGAUCAAUAGUACCCGUAUUAACGGGUGA"
        let (Just actual) = fromRna rna
        show actual `shouldBe` "MAMAPRTEINSTRING"
