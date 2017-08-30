module Main where

import System.IO
import Data.List
import Control.Arrow

trim :: String -> String
trim = reverse . dropWhile (== '\n') . reverse . filter (/= '\r')

readFasta :: String -> [(String, String)]
readFasta = f . words
  where f = foldl' g []
        g a ('>':xs)  = (xs, "") : a
        g ((r, rs):ys) x = (r, rs ++ x) : ys
        g _ _ = []

overlapping :: String -> String -> Int -> Bool
overlapping n m k = suffix `isSuffixOf` n
  where suffix = take k m

cartesian :: [a] -> [b] -> [(a, b)]
cartesian xs ys = concatMap (\x -> map (\y -> (x, y)) ys) xs

main :: IO ()
main = do
  handle <- openFile "rosalind_grph.txt" ReadMode
  contents <- hGetContents handle

  let k = 3
  let fasta = readFasta $ trim contents
  let graph = map (fst *** fst)
            $ filter (\(x,y) -> overlapping (snd x) (snd y) k)
            $ filter (uncurry (/=))
            $ cartesian fasta fasta
  print graph
  hClose handle
