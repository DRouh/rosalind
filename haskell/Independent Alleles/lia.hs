{-# LANGUAGE FlexibleContexts #-}
module Main where

combination n k = foldr (\s i -> s * (n - i + 1 / i)) 1 [1..k]

bernoulliTrial n i s f =
  combination n i * (s ** i) * (f ** (n - i))

probabilityOfAtLeast gens num =
  let total = 2 ** gens
      probs = map (\i -> bernoulliTrial total i 0.25 0.75) [0..num - 1]
  in (1.0 - sum probs)

main :: IO ()
main = do
  let generations = 2 :: Double
  let number = 1 :: Double
  let probability = probabilityOfAtLeast generations number
  print probability
