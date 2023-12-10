import Data.Char 
import Text.Read (readMaybe)

main = do
    putStrLn "hello, what's your name?"
    name <- getLine 
    putStrLn ("Hey " ++ name ++ "!")


recover_calibration_value :: String -> Int
recover_calibration_value input =
    read string_digits :: Int
    where digits = get_digits input
          string_digits = (head digits):[(last digits)] 

get_digits :: String -> String
get_digits input = filter isDigit input

