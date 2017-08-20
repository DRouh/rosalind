module Main where

import System.IO
import Control.Monad (unless)

main :: IO ()
main = do
  handle <- openFile "rosalind_revc.txt" ReadMode
  contents <- hGetContents handle
  let chars = filter (/= '\n') contents
  let complemented = complement chars
  unless (null complemented) $
        writeFile "ouput.txt" complemented
  hClose handle

complement :: String -> String
complement = map (\x -> case x of
                        'A' -> 'T'
                        'C' -> 'G'
                        'T' -> 'A'
                        'G' -> 'C'
                        _ -> x
                 ) . reverse
