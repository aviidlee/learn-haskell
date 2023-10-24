-- find the largest number under 100,000 that's divisible by 3829
-- filter (\n -> n `mod` 3829 == 0) [1..100000] -- gives us the whole list 
largestDivisible :: (Integral a) => a
largestDivisible = head (filter p [100000,99999..])
    where p x = x `mod` 3829 == 0

-- Sum of all odd squares less than 10000
-- all squares less than 10000
squaresLessThan10k = [x^2 | x <- [1..], x^2 < 10000]
-- this one will never finish because it will still continue evaluating the predicate forever 
-- sum(filter (\x -> x^2 < 10000 && x^2 `mod` 2 == 1) [1..])
-- sum(takeWhile (< 10000) (filter (\x -> x `mod` 2 == 1) [x^2 | x <- [1..]]))

-- or using maps 
yolo = sum (takeWhile (< 10000) (filter odd (map (^2) [1..])))

-- Collatz sequence
-- for all starting numbers between 1 and 100, how many chains have a length greater than 15?
chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n 
    | even n = n:chain (n `div` 2)
    | odd n = n:chain (n*3 + 1)

numLongChains :: Int 
numLongChains = length (filter isLong (map chain [1..100]))
    where isLong xs = length xs > 15
