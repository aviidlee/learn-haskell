{-# LANGUAGE OverloadedStrings #-}
module Acronym (abbreviate) where

import qualified Data.Text as T
import           Data.Text (Text)
import Data.Char (isPunctuation, toUpper, isLower, isUpper) 

abbreviate :: String -> String
abbreviate xs = map toUpper ((takeFirst . splitWords) (removePunct (T.pack xs)))

removePunct :: T.Text -> T.Text 
removePunct word = T.filter (not . isPunctuation) (T.replace "-" " " word)

takeFirst :: [String] -> [Char]
takeFirst words = map head words

splitWords :: Text -> [String] 
splitWords phrase = foldr (\word acc -> splitCamel word ++ acc) [] (map T.unpack  (T.words phrase))

splitCamel :: String -> [String]
splitCamel [] = []
splitCamel (x:xs)
  | all isUpper (x:xs) = [x:xs] 
  | otherwise = [x : lowerCase] ++ (splitCamel rest)
      where lowerCase = fst (span isLower xs)
            rest = snd (span isLower xs)