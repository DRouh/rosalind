module RosalindUtils.Protein
    (
      AminoAcid (..),
      Protein (Protein),
      createProtein
    ) where

import Text.Read

newtype Protein =
  Protein [AminoAcid] deriving (Eq, Ord)

instance Show Protein where
  show (Protein acids) = concatMap show acids

data AminoAcid =
  A | R | N | D | C | E | Q | G | H | I | L | K |
  M | F | P | S | T | W | Y | V | Stop
  deriving (Read, Show, Eq, Ord)

createProtein :: String -> Maybe Protein
createProtein s =
  Protein <$> mapM readAminos strings
  where strings = [[c] :: String | c <- s]
        readAminos x = readMaybe x :: Maybe AminoAcid
