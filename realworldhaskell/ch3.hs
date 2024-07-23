-- https://book.realworldhaskell.org/read/defining-types-streamlining-functions.html

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

