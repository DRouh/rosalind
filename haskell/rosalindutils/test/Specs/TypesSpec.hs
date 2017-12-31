module Specs.TypesSpec where

import Data.Maybe

import Test.Hspec
import Test.QuickCheck
import RosalindUtils

genProteinChar :: Gen Char
genProteinChar =
  elements ['A', 'R', 'N', 'D', 'C', 'E', 'Q', 'G', 'H', 'I', 'L', 'K', 'M', 'F', 'P', 'S', 'T', 'W', 'Y', 'V']

genRnaChar :: Gen Char
genRnaChar = elements ['A', 'C', 'G', 'U']

genDnaChar :: Gen Char
genDnaChar = elements ['A', 'C', 'G', 'T']

genString :: Gen Char -> Gen String
genString = listOf

spec :: Spec
spec =
  describe "Types" $ do
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
      it "should create a RNA from any string of A,C,G,U letters" $
        forAll (genString genRnaChar) $ \x -> isJust $ createRna x
      it "should not change RNA during RNA -> DNA -> RNA" $
        forAll (genString genRnaChar) $ \x -> do
          let rna' = fromJust $ createRna x
          dnaToRna (rnaToDna rna') == rna'

    describe "DNA" $ do
      it "should create RNA from DNA" $
        dnaToRna dna `shouldBe` rna
      it "should display RNA as its string" $
        show dna `shouldBe` inputDna
      it "should create a DNA from any string of A,C,G,T letters" $
        forAll (genString genDnaChar) $ \x -> isJust $ createDna x
      it "should not change DNA during DNA -> RNA -> DNA" $
        forAll (genString genDnaChar) $ \x -> do
          let dna' = fromJust $ createDna x
          rnaToDna (dnaToRna dna') == dna'

    describe "Protein" $ do
      it "should create Protein from DNA" $
        fromDna dna `shouldBe` Just protein
      it "should display Protein as its string" $
        show protein `shouldBe` inputProtein
      it "should create a Protein from any string of Protein letters" $
        forAll (genString genProteinChar) $ \x -> isJust $ createProtein x
