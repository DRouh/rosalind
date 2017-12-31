module Specs.TypesSpec where

import Data.Maybe

import Test.Hspec
import RosalindUtils

spec :: Spec
spec =
  describe "utils" $ do
    let inputProtein = "MAMAPRTEINSTRING"
    let inputRna = "AUGGCCAUGGCGCCCAGAACUGAGAUCAAUAGUACCCGUAUUAACGGGUGA"
    let inputDna = "ATGGCCATGGCGCCCAGAACTGAGATCAATAGTACCCGTATTAACGGGTGA"

    let (Just protein) = createProtein inputProtein
    let (Just rna) = createRna inputRna
    let (Just dna) = createDna inputDna

    describe "RNA" $ do
      it "should create protein from RNA" $
        show (fromJust $ fromRna rna) `shouldBe` inputProtein
      it "should display RNA as its string" $
        show rna `shouldBe` inputRna

    describe "DNA" $ do
      it "should create RNA from DNA" $
        dnaToRna dna `shouldBe` rna
      it "should display RNA as its string" $
        show dna `shouldBe` inputDna

    describe "Protein" $ do
      it "should create Protein from DNA" $
        fromDna dna `shouldBe` Just protein
      it "should display Protein as its string" $
        show protein `shouldBe` inputProtein
