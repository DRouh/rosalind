module Main where

import System.IO
import Data.List
import Data.List.Split
import Control.Monad
import qualified Data.Map as M
import Data.Maybe (isJust, maybe, fromMaybe)
import Text.Regex.PCRE
import Data.Array ((!))

type CodonLookup = M.Map String String

readFasta :: String -> [(String, String)]
readFasta = f . words
  where f = foldl' g []
        g a ('>':xs)  = (xs, "") : a
        g ((r, rs):ys) x = (r, rs ++ x) : ys
        g _ _ = []

occurences :: String -> String -> [Int]
occurences p s =
  let re = makeRegex p :: Regex
  in map (fst . (!0)) $ matchAll re s

complementDna :: String -> String
complementDna = map (\x -> case x of
                        'A' -> 'T'
                        'C' -> 'G'
                        'T' -> 'A'
                        'G' -> 'C'
                        _ -> x
                 ) . reverse

readCodonTable :: [String] -> Maybe CodonLookup
readCodonTable = (M.fromList <$>) . sequence . ((pair . words) <$>)
   where pair [a, b] = Just (a, b)
         pair _ = Nothing

startDnaCodons :: [String]
startDnaCodons = ["ATG"]

takeWhileInclusive :: (a -> Bool) -> [a] -> [a]
takeWhileInclusive _ [] = []
takeWhileInclusive p (x:xs) =
  x : if p x then takeWhileInclusive p xs else []

possibleORFToProtein :: CodonLookup -> [String] -> [String]
possibleORFToProtein l =
  let mapDnaCodon = flip M.lookup l
      toDnaCodons =
        sequence . takeWhile isJust . map mapDnaCodon . chunksOf 3
      orfToProtein =
        takeWhileInclusive notStop . fromMaybe [] .toDnaCodons
      notStop = (/=) "Stop"
      filterNonValid = filter (\x -> last x == "Stop") . filter (not . null)
      format = concat . takeWhile notStop
  in map format . filterNonValid . map orfToProtein

findPossibleORFs :: [String] -> String -> [String]
findPossibleORFs start s =
  map (`drop` s) $ nub $ filter (>= 0) $ concatMap (`occurences` s) start

findProteins :: CodonLookup -> [String] -> String -> [String]
findProteins l startDnaCodons dna =
  let find = possibleORFToProtein l . findPossibleORFs startDnaCodons
      proteinsFromDna = find dna
      proteinsFromComplementedDna = find $ complementDna dna
  in nub $ proteinsFromDna ++ proteinsFromComplementedDna

main :: IO ()
main = do
  handleTable <- openFile "dna_codon_table.txt" ReadMode
  codonList <- hGetContents handleTable
  handle <- openFile "rosalind_orf.txt" ReadMode
  dnaContent <- hGetContents handle

  let table = fromMaybe M.empty $ readCodonTable $ lines codonList
  let dna = (\x -> if null x then "" else snd $ head x) $ readFasta dnaContent
  let result = findProteins table startDnaCodons dna
  print result
  hClose handleTable
  hClose handle
