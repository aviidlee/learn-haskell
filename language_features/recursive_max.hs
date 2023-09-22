m :: (Ord a) => [a] -> a 
m [] = error "Empty list "
m [x] = x 
m (x:xs) = max x (m xs)

-- Produce a list which consists of `repeat` copies of `elem` 
repl :: (Num a, Ord a) => a -> a -> [a]
repl elem repeat 
    | repeat <= 0 = []
    | otherwise = elem:repl elem (repeat-1) 

-- Return a list of the first n elements of a list 
yank :: (Num n, Ord n) => n -> [a] -> [a]
yank n _ 
    | n <= 0 = []
yank n list = case (n, list) of (_, []) -> []
                                (n, first:rest) -> first : yank (n-1) rest
                                