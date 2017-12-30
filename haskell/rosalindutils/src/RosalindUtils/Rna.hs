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

newtype Rna = Rna [RnaNucleo] deriving (Show, Eq, Ord)

createRna :: String -> Maybe Rna
createRna s =
  Rna <$> mapM readNucleos strings
  where strings = [[c] :: String | c <- s]
        readNucleos x = readMaybe x :: Maybe RnaNucleo
