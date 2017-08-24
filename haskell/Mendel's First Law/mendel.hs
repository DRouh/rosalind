module Main where

prob :: Int -> Int -> Int -> Double
prob k m n = sum probabilities / (total * (total - 1.0))
  where kf = fromIntegral k
        mf = fromIntegral m
        nf = fromIntegral n
        total = kf + mf + nf
        probabilities = [2.00 * kf * mf,
                         2.00 * kf * nf,
                         1.00 * kf * (kf - 1.0),
                         1.0 * mf * nf,
                         0.75 * mf * (mf - 1.0)]

main :: IO ()
main = do
  let k = 2 -- homozygous dominant: XX
  let m = 2 -- heterozygous: Xx
  let n = 2 -- homozygous recessive: xx
  let ps = prob k m n
  print ps
