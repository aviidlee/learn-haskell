-- https://exercism.org/tracks/haskell/exercises/nucleotide-count

import Data.Map (Map)

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show)

-- nucleotideCounts :: String -> Either String (Map Nucleotide Int)
nucleotideCounts xs = nucleotide 
  nucleotide 
  where x = head xs
        rest = tail xs
        nucleotide = 
          case x of 
            'A' -> A
            'C' -> C
            'G' -> G
            'T' -> T
            _   -> error "Not a valid DNA sequence"
        