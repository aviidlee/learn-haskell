-- From https://exercism.org/tracks/haskell/exercises/anagram

import Data.Char (toLower)
import qualified Data.Map as Map

anagramsFor :: String -> [String] -> [String]
anagramsFor "" _ = []
anagramsFor _ [] = [] 
anagramsFor target candidates = filter (\word -> isAnagram target word) candidates

isAnagram :: String -> String -> Bool  
isAnagram target word
  | targetLowerCase == wordLowerCase = False
  | otherwise = (letterComposition targetLowerCase) == (letterComposition wordLowerCase) 
  where targetLowerCase = map toLower target 
        wordLowerCase = map toLower word 

letterComposition :: String -> Map.Map Char Int
letterComposition word = undefined
