module Main where

import System.IO

expected :: [Int] -> Double
expected n = (* 2.0) $ sum probs
  where probs = zipWith (*) nd [1.0, 1.0, 1.0, 0.75, 0.5, 0.0]
        nd = map fromIntegral n

main :: IO ()
main = do
  handle <- openFile "rosalind_iev.txt" ReadMode
  contents <- hGetContents handle
  let nums = map (read::String->Int) $ words contents
  let e = expected nums
  print e
  hClose handle
