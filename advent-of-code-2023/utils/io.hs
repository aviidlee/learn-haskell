import System.IO

getLines :: String -> IO([String])
getLines fileName = do 
    handle <- openFile fileName ReadMode 
    contents <- hGetContents handle  
    return (lines contents) 