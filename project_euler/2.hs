-- naive recursive solution is too slow, need dynamic programming.
let fib 1 = 1
    fib 2 = 2
    fib n = fib (n-1) + fib (n-2)
smallFibs = [fib(n) | n <- [1,2..], fib(n) < 4000000]
