import Data.Char 
import Data.List
import System.IO
import System.Environment
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe

numbers = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
numbers_to_int = Map.fromList $ zip numbers [1..]

main = do
    args <- getArgs
    handle <- openFile (args !! 0) ReadMode
    contents <- hGetContents handle 
    let values = lines contents
        recovered_values = map recover_calibration_value values 
        answer = sum recovered_values
    print answer


get_answer :: [String] -> Int
get_answer input = do
    sum recovered_values 
    where recovered_values = map recover_calibration_value input 

    
recover_calibration_value :: String -> Int
recover_calibration_value input =
    read string_digits :: Int 
    where digits = get_numbers input
          string_digits = (normalize (head digits)):[(normalize (last digits))]


normalize :: String -> Char 
normalize string 
    | isDigit (string !! 0) = string !! 0
    | otherwise = intToDigit (fromJust (Map.lookup string numbers_to_int))


get_numbers :: String -> [String] 
get_numbers input
    | input == [] = []
    | maybe_number == [] = get_numbers (tail input) 
    | otherwise = maybe_number ++ (get_numbers (tail input))
    where maybe_number = get_head_if_number input


get_head_if_number :: String -> [String] 
get_head_if_number input = get_head_if_digit input ++ get_head_if_number_in_words input 


get_head_if_digit :: String -> [String] 
get_head_if_digit (x:xs)
    | isDigit x = [[x]]
    | otherwise = []


get_head_if_number_in_words :: String -> [String]
get_head_if_number_in_words input = 
    filter (flip isPrefixOf input) numbers 
