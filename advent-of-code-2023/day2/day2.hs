import Data.Char 
import Data.List
import System.IO
import System.Environment

main = do
    args <- getArgs
    handle <- openFile (args !! 0) ReadMode
    contents <- hGetContents handle 
    let values = lines contents
        recovered_values = map recover_calibration_value values 
        answer = sum recovered_values
    print answer

recover_calibration_value :: String -> Int
recover_calibration_value input =
    read string_digits :: Int
    where digits = get_digits input
          string_digits = (head digits):[(last digits)] 


get_digits :: String -> String
get_digits input = "not yet implemented"

get_first_number :: String -> String 
get_first_number input 
    | maybe_number == [] = get_first_number (tail input)
    | otherwise = maybe_number !! 0
    where maybe_number = get_head_if_number input

get_head_if_number :: String -> [String] 
get_head_if_number input = get_head_if_digit input ++ get_head_if_number_in_words input 


get_head_if_digit :: String -> [String] 
get_head_if_digit (x:xs)
    | isDigit x = [[x]]
    | otherwise = []


get_head_if_number_in_words :: String -> [String]
get_head_if_number_in_words input = 
    filter (flip isPrefixOf input) nums 
    where nums = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
