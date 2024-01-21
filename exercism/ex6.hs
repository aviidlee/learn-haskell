-- https://exercism.org/tracks/haskell/exercises/collatz-conjecture

-- First attempt solution
collatz :: Integer -> Maybe Integer 
collatz n 
    | n >= 1 = Just (snd (collatz' n 0))
    | otherwise = Nothing

collatz' :: Integer -> Integer -> (Integer, Integer)
collatz' 1 steps = (1, steps)
collatz' n steps
    | n `mod` 2 == 0 = collatz' (n `div` 2) (steps+1)
    | otherwise = collatz' (3*n + 1) (steps+1)


-- "Dig deeper" iterative solution
collatz2 :: Integer -> Maybe Integer
    | n < 1 = Nothing 
    | otherwise = Just(countSteps n)
    where countSteps = toInteger . length . takeWhile (/=1) . iterate nextStep 
          nextStep k = if even k then k `div` 2 else 3 * k + 1