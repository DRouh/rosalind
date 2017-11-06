{-# LANGUAGE BangPatterns #-}
module Codon (CodonLookup, getCodonTable) where

import System.IO
import qualified Data.Map as M
import Data.Maybe (fromMaybe)

type CodonLookup = M.Map String String

readCodonTable :: [String] -> Maybe CodonLookup
readCodonTable = (M.fromList <$>) . sequence . ((pair . words) <$>)
   where pair [a, b] = Just (a, b)
         pair _ = Nothing

getCodonTable :: IO CodonLookup
getCodonTable = do
  handleTable <- openFile "codon_table.txt" ReadMode
  codonList <- hGetContents handleTable
  let table = readCodonTable $ lines codonList
  let! a = fromMaybe M.empty table
  hClose handleTable
  return a
