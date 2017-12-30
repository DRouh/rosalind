module RosalindUtils.Types
    (
      AminoAcid (..),
      RnaCodon (RnaCodon),
      Protein (Protein)
    ) where

newtype Protein =
  Protein [AminoAcid] deriving (Eq, Ord)

instance Show Protein where
  show (Protein acids) = concatMap show acids

data AminoAcid =
  A | R | N | D | C | E | Q | G | H | I | L | K |
  M | F | P | S | T | W | Y | V | Stop
  deriving (Read, Show, Eq, Ord)

newtype RnaCodon =
  RnaCodon String deriving (Show, Eq, Ord)
