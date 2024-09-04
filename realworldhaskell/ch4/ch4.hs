import Data.List (tails)

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

-------------------------
--- Partial Functions ---
-------------------------

-- When we pass fewer arguments to a function than the function can accept, we call it __partial application__ of the function
-- Also called __currying__
-- nicerSum = foldl (+) 0

--- Sections ---
-- __section__ = partial function application done infix style by putting parentheses 
-- E.g.
addOne_section = (1+)
-- instead of 
addOne = (+) 1
-- Using back-ticks, we can do this with other functions:
-- Notice putting the function on the left means we are fixing its second parameter  
isLowercaseAlphabetical = (`elem` ['a'..'z'])

--- As-patterns --- 
-- `@` means "bind the variable that matches the value on the left side of @ to what matches the pattern on the right side"
-- xs@(_:xs') binds xs to the entire list if it matches the expression (_:xs'), and xs' is bound to the tail of that list.
suffixes :: [a] -> [[a]]
suffixes xs@(_:xs') = xs : suffixes xs' 
suffixes _ = []

-- this helps to share data instead of copying it; if we repeated (x:xs) on the right side, it would allocate a new list

---- Function Composition ----
-- `.` is RIGHT associative
suffixes3 = init . tails  

-- Count the number of words in a string that begin with a capital letter
-- 1. break into words, e.g. ["I", "play", "bagpipes"]
-- 2. identify those starting with capital letters
-- 3. count them. 
countCapitalWords xs = length $ filter startsWithCaps (words xs)
  where startsWithCaps = (`elem` ['A'..'Z']) . head

-- book soln
-- :module +Data.Char
-- let capCount = length . filter (isUpper . head) . words

