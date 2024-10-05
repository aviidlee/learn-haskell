module Prettify 
where 

-- Instead of rendering straight to string, use an intermediate called Doc

import SimpleJSON

data Doc = ToBeDefined deriving (Show)

string :: String -> Doc 
string str = undefined 

text :: String -> Doc 
text str = undefined 

compact :: Doc -> String
compact doc = undefined

double :: Double -> Doc 
double num = undefined

char :: Char -> Doc 
char c = undefined 

(<>) :: Doc -> Doc -> Doc 
a <> b = undefined 

hcat :: [Doc] -> Doc 
hcat xs = undefined

pretty :: Doc -> String 
pretty doc = undefined

fsep :: [Doc] -> Doc
fsep xs = undefined

punctuate :: Doc -> [Doc] -> [Doc]
punctuate p []     = []
punctuate p [d]    = [d]
punctuate p (d:ds) = (d Prettify.<> p) : punctuate p ds

