module Main where

permutationsFixedHead :: Int -> [[Int]]
permutationsFixedHead n = permutations' [1..n]
  where remove v = filter (v /=)
        permutations' [] = []
        permutations' [v] = [[v]]
        permutations' l =  foldl go [] l
          where go acc v = acc ++ map (\x -> v:x) xs
                   where xs = permutations' (remove v l)

main :: IO ()
main = do
  let n = 3
  let p = permutationsFixedHead n
  print p
