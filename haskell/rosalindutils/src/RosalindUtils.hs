module RosalindUtils
    (
      module RosalindUtils.Fasta,
      Protein,
      D.Dna,
      R.Rna,
      createProtein,
      dnaToRna,
      rnaToDna,
      fromRna,
      fromDna,
      R.createRna,
      D.createDna,
      D.applyToDna
    ) where

import RosalindUtils.Fasta
import RosalindUtils.Protein
import RosalindUtils.Codon
import qualified RosalindUtils.Dna as D
import qualified RosalindUtils.Rna as R

dnaToRna :: D.Dna -> R.Rna
dnaToRna (D.Dna dna) = R.Rna $ map transcribe dna
  where transcribe D.T = R.U
        transcribe x = read $ show x

rnaToDna :: R.Rna -> D.Dna
rnaToDna (R.Rna dna) = D.Dna $ map transcribe dna
  where transcribe R.U = D.T
        transcribe x = read $ show x

fromRna :: R.Rna -> Maybe Protein
fromRna =
  fmap (Protein . f) . mapM rnaToAmino . rnaToCodons
  where f = takeWhile (/= Stop)

fromDna :: D.Dna -> Maybe Protein
fromDna = fromRna . dnaToRna
