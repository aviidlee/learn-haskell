import Data.Char 
import System.IO

main = do
    handle <- openFile "input.txt" ReadMode
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

