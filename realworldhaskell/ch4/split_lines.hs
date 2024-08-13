-- Haskell provides a built-in `lines` but it doesn't deal with Windows vs Unix line endings. 
-- E.g. if you read on a file written in Windows with `\r\n`, you will get trailing `\r` everywhere.
module FixLineEndings where 

fixLineEndings :: String -> String 
fixLineEndings input = unlines (splitLines input)

splitLines :: String -> [String]
splitLines [] = [] 
splitLines cs = 
  -- break returns tuple where first elem the longest prefix of cs which is NOT a line terminator
  -- and the second is the rest 
  let (pre, suf) = break isLineTerminator cs 
  -- the `pre :` adds the prefix to the head of the list the expression evaluates to 
  in pre : case suf of 
    ('\r': '\n':rest) -> splitLines rest 
    ('\r':rest)       -> splitLines rest 
    ('\n':rest )      -> splitLines rest 
    _                 -> [] 

isLineTerminator c = c == '\r' || c == '\n'