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
get_digits input = filter isDigit input

starts_with_digit :: String -> [String]
starts_with_digit input = 
    filter (flip isPrefixOf input) nums 
    where nums = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
