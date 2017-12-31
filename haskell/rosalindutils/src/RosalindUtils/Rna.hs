module RosalindUtils.Rna
    (
      Rna (..),
      RnaNucleo (..),
      createRna
    ) where
      
import Text.Read

data RnaNucleo =
  A | C | G | U
  deriving (Read, Show, Eq, Ord)

newtype Rna = Rna [RnaNucleo] deriving (Eq, Ord)

instance Show Rna where
  show (Rna s) = concatMap show s

createRna :: String -> Maybe Rna
createRna s =
  Rna <$> mapM readNucleos strings
  where strings = [[c] :: String | c <- s]
        readNucleos x = readMaybe x :: Maybe RnaNucleo
