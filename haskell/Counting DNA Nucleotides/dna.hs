module Main where

import System.IO
import Data.List

main :: IO ()
main = do
  handle <- openFile "rosalind_dna.txt" ReadMode
  contents <- hGetContents handle
  let chars = filter (/= '\n') contents
  let counts = countUnique chars
  print $ map snd counts
  hClose handle

countUnique :: String -> [(Char, Int)]
countUnique = map (\xs @ (x : _) -> (x, length xs)) . group . sort
