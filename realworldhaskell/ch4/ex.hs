import Data.Char (digitToInt, isDigit)
import Data.List (find)

-- Ex 1 

safeHead :: [a] -> Maybe a
safeHead (x:xs) = Just x 
safeHead [] = Nothing

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing 
safeTail (x:xs) = Just xs 

safeLast :: [a] -> Maybe a
safeLast [] = Nothing 
safeLast xs = Just $ last xs

anotherLast :: [a] -> Maybe a 
anotherLast [] = Nothing 
anotherLast (x:xs) 
  | null xs = Just x
  | otherwise = anotherLast xs 

safeInit :: [a] -> Maybe [a]
safeInit [] = Nothing 
safeInit xs = Just $ init xs

-- Ex 2
-- Write a function splitWith that acts similarly to words, 
-- but takes a predicate and a list of any type, and splits its 
-- input list on every element for which the predicate returns False. 
splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith pred xs =
  prefix : case suffix of 
    [] -> []
    c:cs -> splitWith pred cs
    where (prefix, suffix) = break pred xs 

-- Ex 3
firstOfEachLine :: String -> String 
firstOfEachLine input = 
  unlines (map firstOfLine (lines input))

firstOfLine :: String -> String
firstOfLine line = head $ words line

-- Fold to rewrite asInt function 
asInt_foldl xs = foldl step 0 xs 
  where step acc x
          | isDigit x = sign * (digitToInt x) + (acc * 10) 
          | x == '-' && acc == 0 = 0
          | otherwise = error "Not a number"
          where sign = getSign xs

getSign :: String -> Int
getSign str = case str of
    '-':_ -> -1
    _ -> 1

-- asInt_fold but with Either to allow user to handle errors
type ErrorMessage = String
asInt_either :: String -> Either ErrorMessage Int 
asInt_either xs = case firstNonDigit of 
    Nothing -> Right $ asInt_foldl xs
    Just x -> Left $ ("Non-digit" ++ [x])
  where firstNonDigit = find (\y -> not (isDigit y) && y !=) xs

  