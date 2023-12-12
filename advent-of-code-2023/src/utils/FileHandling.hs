module FileHandling (
    getLines 
) where 

import System.IO

getLines :: Handle -> IO([String])
getLines handle = do 
    contents <- hGetContents handle  
    return (lines contents) 