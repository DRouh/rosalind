module Main where

import System.IO
import Data.List
import Control.Monad (unless)
import Data.Function (on)

readFasta :: String -> [(String, String)]
readFasta = f . words
  where f = foldl' g []
        g a ('>':xs)  = (xs, "") : a
        g ((r, rs):ys) x = (r, rs ++ x) : ys
        g _ _ = []

computeGC :: String -> Double
computeGC x = 100.0 * (gc / total)
  where gc = fromIntegral $ foldl' (+) 0 mapped
        mapped = map (\c -> if c == 'C' || c == 'G' then 1::Int else 0) x
        total = fromIntegral $ length x

mapSnd :: (b -> c) -> (a, b) -> (a, c)
mapSnd f (a, b) = (a, f b)

trim :: String -> String
trim = reverse . dropWhile (== '\n') . reverse . filter (/= '\r')

main :: IO ()
main = do
  handle <- openFile "rosalind_gc.txt" ReadMode
  contents <- hGetContents handle
  let tCOntents = trim contents
  let fasta = readFasta tCOntents
  let gc = map (mapSnd computeGC) fasta

  unless (null gc) $
     print $ maximumBy (compare `on` snd)  gc
  hClose handle
