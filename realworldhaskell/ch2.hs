-- https://book.realworldhaskell.org/read/types-and-functions.html

myDrop :: Int -> [a] -> [a]
myDrop _ [] = []
myDrop 0 l = l
myDrop n l 
    | n < 0 = error "Cannot drop negative elements"
    | otherwise = myDrop (n-1) (tail l)
    
-- Ex 2
lastButOne :: [a] -> a
lastButOne xs = last $ init xs