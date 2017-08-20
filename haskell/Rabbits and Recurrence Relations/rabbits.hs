module Main where

main :: IO ()
main = do
  let grew = grow 35 3
  print grew

grow :: Int -> Integer -> Integer
grow gen productionRate = f gen
     where f 1  = 1
           f 2  = 1
           f x = f (x - 1) + productionRate * f (x - 2)
