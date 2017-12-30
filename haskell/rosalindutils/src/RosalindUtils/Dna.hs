module RosalindUtils.Dna
    (
      Dna (..),
      DnaNucleo (..)
    ) where

data DnaNucleo =
  A | C | G | T
  deriving (Read, Show, Eq, Ord)

newtype Dna = Dna [DnaNucleo] deriving (Show, Eq, Ord)
