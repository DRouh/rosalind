{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Maybe
import Data.Text
import qualified Data.List as L
import Control.Monad
import System.IO

import RosalindUtils

excludeIntron :: Dna -> Text -> Maybe Dna
excludeIntron d what = applyToDna f d
  where f = replace what ""

slice :: String -> [String] -> Maybe Protein
slice dnaString introns = do
  dna <- createDna dnaString
  dnaWithoutIntrons <- foldM excludeIntron dna $ L.map pack introns
  fromDna dnaWithoutIntrons
  
main :: IO ()
main = do
  handle <- openFile "rosalind_splc.txt" ReadMode
  contents <- hGetContents handle
  let (dnaString:introns) = L.map snd $ readFasta contents
  let protein = slice dnaString introns
  print protein
