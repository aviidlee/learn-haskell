-- https://exercism.org/tracks/haskell/exercises/rna-transcription

import Data.Either (isLeft, fromRight, fromLeft)

-- First attempt, seems very tedious
toRNA :: String -> Either Char String
toRNA dna = foldl addIfRight (Right "") (map transcribeLetter dna)

addIfRight :: Either Char String -> Either Char Char -> Either Char String
addIfRight accumulator either
    | isLeft accumulator = accumulator 
    | isLeft either = Left $ fromLeft 'a' either 
    | otherwise = Right (fromRight "" accumulator ++ [fromRight 'a' either])

transcribeLetter :: Char -> Either Char Char 
transcribeLetter x
    | x == 'G' = Right 'C' 
    | x == 'C' = Right 'G'
    | x == 'T' = Right 'A'
    | x == 'A' = Right 'U'
    | otherwise = Left x
 