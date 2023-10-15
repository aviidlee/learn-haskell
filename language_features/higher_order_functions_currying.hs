-- Zipwith; takes 2 lists as parameters and joins them together by applying the function between corresponding elements.  
zipWth :: (a -> b -> c) -> [a] -> [b] -> [c]
-- if either list is empty, return an empty list
zipWth _ [] _ = []
zipWth _ _ [] = []
zipWth f (x:xs) (y:ys) = f x y : zipWth f xs ys 

add :: (Num a) => a -> a -> a
add x y = x + y

-- Flip
-- Takes a function and returns a function that is like the original, but with the first 2 arguments flipped
flippy :: (a -> b -> c) -> (b -> a -> c)
flippy f x y = (f y) x 

-- function to test flippy 
first :: a -> b -> a
first x y = x