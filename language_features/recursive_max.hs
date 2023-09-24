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

-- Reverse a list 
backwards :: [a] -> [a]
backwards list = case list of [] -> []
                              (first:rest) -> backwards rest ++ [first]

-- Zip 
zip' :: [a] -> [b] -> [(a,b)] 
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x, y) : zip' xs ys

-- elem 
find :: (Eq a) => a -> [a] -> Bool 
find _ [] =  False 
find x (y:ys) 
    | x == y = True 
    | otherwise = find x ys

-- Quicksort 
qsort :: (Ord a) => [a] -> [a]
qsort [] = [] 
qsort (x:xs) = qsort [a | a <- xs, a <= x] ++ [x] ++ [a | a <- xs, a > x]