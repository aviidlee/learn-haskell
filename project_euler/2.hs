-- naive recursive solution is too slow, need dynamic programming.
fib =
    let fib' 1 = 1
        fib' 2 = 2
        fib' n = fib (n-1) + fib (n-2)
    in (map fib' [1..] !!)
    
smallFibs = takeWhile (<4000000) [fib(n) | n <- [1,2..]]
smallEvenFibs = filter (\x -> x `mod` 2 == 0) smallFibs  
summed = sum smallEvenFibs