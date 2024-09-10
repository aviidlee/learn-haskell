--  To create an executable, ghc expects a module named Main that contains a function named main
-- Run ghc Main.hs SimpleJson.hs
module Main (main) where 

import SimpleJSON

main = print (JObject [("foo", JNumber 1), ("bar", JBool False)])