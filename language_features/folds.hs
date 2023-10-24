-- foldl, called the left fold, applies the binary function between the starting value and head of list

sum2 :: (Num a) => [a] -> a
sum2 xs = foldl (\acc x -> acc + x) 0 xs 

elem2 :: (Foldable t, Eq a) => a -> t a -> Bool
elem2 e list = foldl (\acc x -> acc || (x == e)) False list

-- use foldr
map2 :: (a -> b) -> [a] -> [b]
map2 xs func = foldr (\x acc -> f x : acc) [] xs 