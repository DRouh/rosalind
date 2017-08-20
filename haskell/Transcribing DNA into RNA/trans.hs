module Main where

import System.IO

main :: IO ()
main = do
  handle <- openFile "rosalind_rna.txt" ReadMode
  contents <- hGetContents handle
  let chars = filter (/= '\n') contents
  let transcribed = transcribe chars
  print transcribed
  hClose handle

transcribe :: String -> String
transcribe = map (\x -> if x == 'T' then 'U' else x)
