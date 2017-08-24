module Main where

import System.IO
import Data.List
import Data.List.Split
import Control.Monad
import qualified Data.Map as M

type CodonLookup = M.Map String String

readCodonTable :: [String] -> Maybe CodonLookup
readCodonTable = (M.fromList <$>) . sequence . ((pair . words) <$>)
   where pair [a, b] = Just (a, b)
         pair _ = Nothing

rnaStringToCodon :: [String]  -> M.Map String String -> Maybe String
rnaStringToCodon = (h .) . (g .) . f
  where f = (sequence .) . mapM M.lookup
        g = fmap (takeWhile (/= "Stop"))
        h = fmap concat

main :: IO ()
main = do
  handleTable <- openFile "codon_table.txt" ReadMode
  codonList <- hGetContents handleTable
  handle <- openFile "rosalind_rna.txt" ReadMode
  rnaContent <- hGetContents handle

  let rna = chunksOf 3 $ filter (/= '\n') rnaContent
  let table = readCodonTable $ lines codonList
  let codon = join $ rnaStringToCodon rna <$> table
  print codon
  hClose handleTable
  hClose handle
