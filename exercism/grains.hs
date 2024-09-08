-- https://exercism.org/tracks/haskell/exercises/grains

import Data.Maybe (fromJust)

square :: Integer -> Maybe Integer
square n 
  | n >= 1 && n <= 64 = Just (2 ^ (n-1))
  | otherwise = Nothing

total :: Integer
total = sum (map fromJust (map square [1..64])) 
-- Or just 2^64 - 1 :) 