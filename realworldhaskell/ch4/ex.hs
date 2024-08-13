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