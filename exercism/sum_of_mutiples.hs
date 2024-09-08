-- https://exercism.org/tracks/haskell/exercises/sum-of-multiples
multiplesLessThan :: Integer -> Integer -> [Integer]
multiplesLessThan number max = 
  takeWhile (<max) (map (*number) [1..])

multiples :: [Integer] -> Integer -> [[Integer]]
multiples factors limit = map (`multiplesLessThan` limit) factors

-- Attempt 1: exercism does not have `Set`
-- import qualified Data.Set as Set
-- sumOfMultiples :: [Integer] -> Integer -> Integer
-- sumOfMultiples factors limit = 
--   sum (foldr (\s acc -> Set.union s acc) (Set.fromList []) (map Set.fromList (multiples factors limit)))  
 
-- Attempt 2: runs too slowly
union :: Eq a => [a] -> [a] -> [a]
union [] ys = ys
union xs [] = xs
union xs@(x:xt) ys 
  | x `elem` ys = union xt ys 
  | otherwise = x : union xt ys

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit = 
  sum (foldr (\s acc -> union s acc) [] (multiples factors limit))