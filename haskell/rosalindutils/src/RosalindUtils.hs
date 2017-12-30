module RosalindUtils
    (
      module RosalindUtils.Types,
      dnaToRna,
      fromRna,
      fromDna,
      R.createRna,
    ) where

import RosalindUtils.Types
import RosalindUtils.Codon
import qualified RosalindUtils.Dna as D
import qualified RosalindUtils.Rna as R

dnaToRna :: D.Dna -> R.Rna
dnaToRna (D.Dna dna) = R.Rna $ map transcribe dna
  where transcribe D.T = R.U
        transcribe x = read $ show x

fromRna :: R.Rna -> Maybe Protein
fromRna =
  fmap (Protein . f) . mapM rnaToAmino . rnaToCodons
  where f = takeWhile (/= Stop)

fromDna :: D.Dna -> Maybe Protein
fromDna = fromRna . dnaToRna
