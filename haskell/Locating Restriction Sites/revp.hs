#!/usr/bin/env stack
-- stack --install-ghc runghc --package ilist
module Main where

import System.IO
import qualified Data.List as L
import Data.List.Split
import Data.List.Index
import Data.Maybe (mapMaybe)
import Control.Arrow (first)

data StringGroup = StringGroup { len :: Int
                               , subGroup :: [SubGroup]
                               } deriving (Show, Eq)
data SubGroup    = SubGroup { position :: Int
                            , string :: String
                            } deriving (Show, Eq)

complement :: String -> String
complement = map (\x -> case x of
                        'A' -> 'T'
                        'C' -> 'G'
                        'T' -> 'A'
                        'G' -> 'C'
                        _ -> x
                 ) . reverse

substring :: Int -> Int -> String -> String
substring i t xs = take t $ drop i xs

readFasta :: String -> [(String, String)]
readFasta = f . words
  where f = L.foldl' g []
        g a ('>':xs)  = (xs, "") : a
        g ((r, rs):ys) x = (r, rs ++ x) : ys
        g _ _ = []

createStringGroup :: Int -> [(Int, String)] -> StringGroup
createStringGroup l xs =
  StringGroup l (map (uncurry SubGroup) xs)

chunks :: Int -> Int -> Int -> String -> [StringGroup]
chunks lB uB i s = map createLengthGroup [lB..upperBound]
  where upperBound = if L.length s - uB > 0 then uB + 1 else lB
        createLengthGroup n = createStringGroup n subgroups
          where ixs = map (\(j, s') -> (i + j * n, s')) $ indexed $ chunksOf n s
                subgroups = filter ((== n) . L.length . snd) ixs

findEqualsInGroup :: String -> StringGroup -> [(Int, Int)]
findEqualsInGroup s StringGroup { len = l, subGroup = xs } =
  let l' = L.length s
      cmp SubGroup { position = p, string = reference } =
          if reference == substring (l' - p - l) l s
            then Just(p, l) else Nothing
  in mapMaybe cmp xs

findReversePalindromes :: String -> Int -> Int -> [(Int, Int)]
findReversePalindromes dna lB uB =
  let complementedDna = complement dna
      notShortTails = takeWhile (\x -> L.length x >= lB) . L.tails
      groups = concatMap (uncurry (chunks lB uB)) . indexed . notShortTails
      pretty = L.sort . map (first (+ 1)) . L.nub
  in pretty $ concatMap (findEqualsInGroup complementedDna) $ groups dna

main :: IO ()
main = do
  handle <- openFile "rosalind_revp.txt" ReadMode
  dnaContent <- hGetContents handle
  let input = snd $ head $ readFasta dnaContent
  let output = findReversePalindromes input 4 12
  print output
  hClose handle
