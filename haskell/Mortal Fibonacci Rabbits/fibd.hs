module Main where

import Data.List

main :: IO ()
main = do
  let grew = numRabbits 87 20
  print grew

numRabbits :: Int -> Int -> Int
numRabbits 0 _ = 0
numRabbits 1 _ = 1
numRabbits n m = sum alive
  where alive = foldl' f initialPopulation gen
        f population _ = next population
        next xs = sum (tail xs) : init xs
        initialPopulation = 1 : replicate (m - 1) 0
        gen = [2..n]

--naive
grow :: Int -> Int -> Integer
grow gen m = f gen
     where f 0  = 1
           f 1  = 1
           f x | x <= m     = f (x - 1) + f (x - 2)
               | x == m + 1 = f (x - 1) + f (x - 2) - 1
               | otherwise  = f (x - 1) + f (x - 2) - f (x - m - 1)
