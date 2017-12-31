module RosalindUtils.Dna
    (
      Dna (..),
      DnaNucleo (..),
      createDna
    ) where

import Text.Read

data DnaNucleo =
  A | C | G | T
  deriving (Read, Show, Eq, Ord)

newtype Dna = Dna [DnaNucleo] deriving (Eq, Ord)

instance Show Dna where
  show (Dna s) = concatMap show s

createDna :: String -> Maybe Dna
createDna s =
  Dna <$> mapM readNucleos strings
  where strings = [[c] :: String | c <- s]
        readNucleos x = readMaybe x :: Maybe DnaNucleo
