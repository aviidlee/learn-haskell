-- https://learning-oreilly-com.res.banq.qc.ca/library/view/real-world-haskell/9780596155339/ch05.html#library_jvalue

-- Compile the module with ghc -c SimpleJson.hs 


-- A source file defines a single module, which is a way of namespacing, and lets us decide what is accessible from outside.
-- The module declaration must preceed all other things in the file 
-- module name must begin with capital letter, and the file must have the same base name
-- the list that follows is a list of exports; if we omit this, every name in the module is exported.
module SimpleJson
  (
    JValue(..) -- the (..) indicates we export the type and all its constructors
    , getString
    , getInt
    , getDouble
    , getBool
    , getObject
    , getArray
    , isNull
    ) where

-- We need JString, JNumber etc. because they're the name of the constructors 
data JValue = JString String
            | JNumber Double
            | JBool Bool
            | JNull
            | JObject [(String, JValue)]
            | JArray [JValue]
              deriving (Eq, Ord, Show)

-- Extract "normal" values from JValue 
-- Use pattern-matching on constructor
getString :: JValue -> Maybe String 
getString (JString s) = Just s
getString _ = Nothing 

getInt (JNumber n) = Just (truncate n)
getInt _ = Nothing 

getDouble (JNumber d) = Just d 
getDouble _ = Nothing 

getBool (JBool bool) = Just bool 
getBool _ = Nothing 

getObject (JObject o) = Just o 
getObject _ = Nothing 

getArray (JArray array) = Just array 
getArray _ = Nothing 

isNull v = v == JNull