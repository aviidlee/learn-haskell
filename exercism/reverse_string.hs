-- https://exercism.org/tracks/haskell/exercises/reverse-string

-- First naive solution: recursion 
reverseString :: String -> String
reverseString [] = []
reverseString xs = last xs : reverseString (init xs)

-- There is in fact, a library function called reverse! 

-- Another way to do the recursion
reverseString' [] = []
reverseString' (x:xs) = reverseString xs ++ [x]