-- https://exercism.org/tracks/haskell/exercises/rna-transcription

import Data.Either (partitionEithers)

toRNA :: String -> Either Char String
toRNA xs 
    | partitionEithers (map (transcribe) xs)

transcribe :: Char -> Either Char Char 
transcribe x
    | x == 'G' = Right 'C' 
    | x == 'C' = Right 'G'
    | x == 'T' = Right 'A'
    | x == 'A' = Right 'U'
    | otherwise = Left x
 