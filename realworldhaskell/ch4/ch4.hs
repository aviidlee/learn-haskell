-- Use of accumulators 
mySum xs = helper 0 xs
  where helper acc (x:xs) = helper (acc + x) xs
        helper acc _ = acc  

-- "Do something to every element of a list and update an accumulator" is
-- a **fold**
-- foldl folds from the left (start)
-- foldr folds from the right (end)
-- foldl :: (a -> b -> a) -> a -> [b] -> a
-- Take a step function, the initial value of the accumulator, and a list, output the final value of accumulator
foldSum xs = foldl (+) 0 xs

