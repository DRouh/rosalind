module RosalindUtils.Codon
    (
      rnaToAmino,
      rnaToCodons,
    ) where
import Data.List.Split
import qualified Data.Map as M
import Control.Arrow

import RosalindUtils.Types
import qualified RosalindUtils.Rna as R

codonTable :: M.Map RnaCodon AminoAcid
codonTable = M.fromList $ map (first RnaCodon) t
  where t =
          [("UAG", Stop), ("UGA", Stop), ("UAA", Stop),
           ("GCU", A), ("GCC", A), ("GCA", A),
           ("GCG", A), ("UGU", C), ("UGC", C),
           ("GAU", D), ("GAC", D), ("GAA", E),
           ("GAG", E), ("UUU", F), ("UUC", F),
           ("GGU", G), ("GGC", G), ("GGA", G),
           ("GGG", G), ("CAU", H), ("CAC", H),
           ("AUU", I), ("AUC", I), ("AUA", I),
           ("AAA", K), ("AAG", K), ("UUA", L),
           ("UUG", L), ("CUU", L), ("CUC", L),
           ("CUA", L), ("CUG", L), ("ACU", T),
           ("ACC", T), ("ACA", T), ("ACG", T),
           ("AAU", N), ("AAC", N), ("AUG", M),
           ("CCU", P), ("CCC", P), ("CCA", P),
           ("CCG", P), ("CAA", Q), ("CAG", Q),
           ("CGU", R), ("CGC", R), ("CGA", R),
           ("CGG", R), ("AGA", R), ("AGG", R),
           ("UCU", S), ("UCC", S), ("UCA", S),
           ("UCG", S), ("AGU", S), ("AGC", S),
           ("GUU", V), ("GUC", V), ("GUA", V),
           ("GUG", V), ("UGG", W), ("UAU", Y),
           ("UAC", Y)]

rnaToAmino :: RnaCodon -> Maybe AminoAcid
rnaToAmino = flip M.lookup codonTable

rnaToCodons :: R.Rna -> [RnaCodon]
rnaToCodons (R.Rna rna) = map (RnaCodon . concatMap show) $ chunksOf 3 rna
