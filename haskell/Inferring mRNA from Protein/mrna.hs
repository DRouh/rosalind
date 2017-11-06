#!/usr/bin/env stack
-- stack --install-ghc runghc
module Main where

import Data.List
import Data.Maybe (fromMaybe)
import qualified Data.Map as M
import Codon (getCodonTable, CodonLookup)

frequencyLookup :: CodonLookup -> M.Map String Int
frequencyLookup m = M.fromListWith (+) . map (\(k, v) -> (v, 1)) $ M.toList m

countReverseTranslations :: CodonLookup -> [String] -> Int
countReverseTranslations codon s =
  let freq = frequencyLookup codon
      lookup = fromMaybe 0 . flip M.lookup freq
      mappings = lookup "Stop" : map lookup s
  in foldl (\acc i -> acc * i `mod` 1000000) 1 mappings

toStrings :: String -> [String]
toStrings = map (:[])

main :: IO ()
main = do
  codon <- getCodonTable
  let input = toStrings "MA"
  let freq = countReverseTranslations codon input
  print freq
