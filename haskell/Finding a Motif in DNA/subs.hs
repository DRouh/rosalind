module Main where

import System.IO

substring :: Int -> Int -> String -> String
substring i t xs = take t $ drop i xs

equalAtIndex :: String -> String -> Int -> Int
equalAtIndex str s i = if substring i (length s) str == s then i else -1

fistLastChIntervalEqual :: String -> Int -> Int -> Char -> Char -> Bool
fistLastChIntervalEqual str i len f l =
  firstEqual && lengthNotExceeded && secondEqual
  where interval = i + len
        firstEqual = (str !! i) == f
        lengthNotExceeded = interval < length str
        secondEqual = str !! (interval - 1) == l

occurences :: String -> String -> [Int]
occurences str sub = filter (>0) $ map (equalAtIndex str sub) indexes
  where indexes = filter g [0..length str - 1]
        g i = fistLastChIntervalEqual str i len (head sub) (last sub)
        len = length sub

main :: IO ()
main = do
  handle <- openFile "rosalind_subs.txt" ReadMode
  contents <- hGetContents handle
  let [input, motif] = lines contents
  let ocs = map (+1) $ occurences input motif
  print ocs
  hClose handle
