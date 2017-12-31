{-# LANGUAGE OverloadedStrings #-}
module RosalindUtils.Dna
    (
      Dna (..),
      DnaNucleo (..),
      createDna,
      applyToDna
    ) where

import Text.Read
import qualified Data.Text as T

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

fromText :: T.Text -> Maybe Dna
fromText = createDna . T.unpack

asText :: Dna -> T.Text
asText (Dna d)  = T.pack $ concatMap show d

applyToDna :: (T.Text -> T.Text) -> Dna -> Maybe Dna
applyToDna f d = fromText $ f (asText d)
