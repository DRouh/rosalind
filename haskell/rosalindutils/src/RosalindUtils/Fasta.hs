module RosalindUtils.Fasta
    (
      readFasta
    ) where

import Data.List
import Text.PortableLines as PL

readFasta :: String -> [(String, String)]
readFasta = reverse . f . PL.lines
  where f = foldl' g []
        g a ('>':xs)  = (xs, "") : a
        g ((r, rs):ys) x = (r, rs ++ x) : ys
        g _ _ = []
