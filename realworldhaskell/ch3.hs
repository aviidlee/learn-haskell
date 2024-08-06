-- https://book.realworldhaskell.org/read/defining-types-streamlining-functions.html
import Data.List (sortBy)


-- Remember in Haskell, the data type name and constructor name can be different.
data BookInfo = Book Int String [String] 
                deriving (Show)

type CustomerID = Int 
type ReviewBody = String 

data BookReview = BookReview BookInfo CustomerID ReviewBody

data Roygbiv = Red
             | Orange
             | Yellow
             | Green
             | Blue
             | Indigo
             | Violet
               deriving (Eq, Show)

type Vector = (Double, Double)

data Shape = Circle Vector Double
           | Poly [Vector] 
             deriving (Show, Eq)

type Power = Int 
type SpellLevel = Int 

data Loot = Sword Int 
    | Shield Int 
	  | WizardStaff Power SpellLevel 
      deriving (Show)
	
lootMessage :: Loot -> String 
lootMessage (Sword attack) = "You got a mighty sword with " ++ (show attack) ++ " attack points!"
lootMessage (Shield defense) = "You get " ++ (show defense) ++ " defense!"
lootMessage (WizardStaff power spellLevel) = "Mighty staff!" 

data List a = Cons a (List a)
            | Nil 
              deriving (Show)

-- Exercise 1
toList (Cons x xs) = x: (toList xs)
toList Nil = []

-- Exercise 2
data Tree a = Node a (Maybe (Tree a)) (Maybe (Tree a))

-- let clause 
withdraw balance amount = let newBalance = balance - amount 
    in if newBalance < 0 
       then Nothing 
       else Just newBalance 

-- where clause 
withdraw_funds balance withdrawal = 
    if newBalance >= 0
    then Just newBalance 
    else Nothing 
    where newBalance = balance - withdrawal 

-- Local functions 
pluralise :: String -> [Int] -> [String]
pluralise word counts = map plural counts
    where plural 0 = "no " ++ word ++ "s"
          plural 1 = "one " ++ word
          plural n = show n ++ " " ++ word ++ "s"

-- length function
len :: [a] -> Integer
len [] = 0
len (x:xs) = len(xs) + 1

-- Mean of a list
mean :: Fractional a => [a] -> Maybe a 
mean [] = Nothing 
mean xs = Just (sum xs / (fromIntegral (len xs)))

-- Turn into palindrome
palindromize :: [a] -> [a]
palindromize xs = xs ++ (reverse xs)

-- isPalindrome
isPalindrome :: Eq a => [a] -> Bool 
isPalindrome xs = (xs == (reverse xs))

-- sort sublists by length
sortByLength :: [[a]] -> [[a]] 
sortByLength xs = sortBy (\l1 l2 -> compare (len l1) (len l2)) xs

-- Join list of lists using separator 
intersperse :: a -> [[a]] -> [a]
intersperse _ [] = []
intersperse _ (x:[]) = x
intersperse sep (x:xs) = x ++ [sep] ++ (intersperse sep xs)

-- Determine height of tree
height :: Tree a -> Integer
height (Node _ Nothing Nothing) = 1  
height (Node _ (Just a) (Just b)) = 1 + (max (height a) (height b))  