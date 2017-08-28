module Main where

import System.IO
import Data.Function (on)
import Data.List

readFasta :: String -> [(String, String)]
readFasta = f . words
  where f = foldl' g []
        g a ('>':xs)  = (xs, "") : a
        g ((r, rs):ys) x = (r, rs ++ x) : ys
        g _ _ = []

trim :: String -> String
trim = reverse . dropWhile (== '\n') . reverse . filter (/= '\r')

sum' :: [Char] -> [Int]
sum' = tupleToList . foldl' plus (0, 0, 0, 0) . map tuplify
  where tuplify 'A' = (1, 0, 0, 0)
        tuplify 'C' = (0, 1, 0, 0)
        tuplify 'G' = (0, 0, 1, 0)
        tuplify 'T' = (0, 0, 0, 1)
        plus (a, b, c, d) (x, y, z, w) =
          (a + x, b + y, c + z, d + w)
        tupleToList (a, c, g, t) = [a, c, g, t]

consensus :: [[Int]] -> String
consensus = map (byIndex . fst . maxBy snd . indexed)
  where byIndex i = ['A', 'C', 'G', 'T'] !! i
        maxBy f = maximumBy (compare `on` f)
        indexed = zip [0..]

main :: IO ()
main = do
  handle <- openFile "rosalind_gc.txt" ReadMode
  contents <- hGetContents handle
  let fasta = map snd
              $ sortBy (compare `on` fst)
              $ readFasta
              $ trim contents
  let matrix = map sum'
               $ transpose fasta
  let con = consensus matrix
  print matrix
  print con
  hClose handle
