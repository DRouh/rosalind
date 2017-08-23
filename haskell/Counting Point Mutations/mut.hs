module Main where

import System.IO

hammingDistance :: String -> String -> Int
hammingDistance a b = sum $ zipWith g a b
   where g x y = if x /= y then 1 else 0

calcDistance :: [String] -> Maybe Int
calcDistance [a, b]
  | length a == length b = Just $ hammingDistance a b
  | otherwise = Nothing
calcDistance _ = Nothing

main :: IO ()
main = do
  handle <- openFile "rosalind_hamm.txt" ReadMode
  contents <- hGetContents handle
  let ls = lines contents
  let hamDist = calcDistance ls
  print hamDist
  hClose handle
