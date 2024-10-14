module Prettify 
(
  Doc, (Prettify.<>), (</>), char, double, fsep, hcat, punctuate, text,
                 compact, pretty, string
)

where

-- Instead of rendering straight to string, use an intermediate called Doc

import SimpleJSON

-- Notice this is a tree
data Doc
  = Empty
  | Char Char
  | Text String
  | Line
  | Concat Doc Doc
  | Union Doc Doc
  deriving (Show)

empty :: Doc
empty = Empty

line :: Doc
line = Line

string :: String -> Doc
string str = Text str

text :: String -> Doc
text str = Text str
text "" = Empty

compact :: Doc -> String
compact x = transform [x]
  where
    transform [] = ""
    transform (d : ds) =
      case d of
        Empty -> transform ds
        Char c -> c : transform ds
        Text s -> s ++ transform ds
        Line -> '\n' : transform ds
        a `Concat` b -> transform (a : b : ds)
        _ `Union` b -> transform (b : ds)


pretty width x = best 0 [x]
  where
    best col (d : ds) =
      case d of
        Empty -> best col ds
        Char c -> c : best (col + 1) ds
        Text s -> s ++ best (col + length s) ds
        Line -> '\n' : best 0 ds
        a `Concat` b -> best col (a : b : ds)
        a `Union` b ->
          nicest
            col
            (best col (a : ds))
            (best col (b : ds))
    best _ _ = ""

    nicest col a b
      | (width - least) `fits` a = a
      | otherwise = b
      where
        least = min width col

-- Ex.1 add spaces to a document until it is the given number of columns wide
-- E.g., using # instead of space to make it visible, we would like:
{-} 
PrettyJSON> putStrLn (pretty 20 (Prettify.fill 30 value))
{"f": 1.0,####################
"q": true#####################
}#############################
-}
fill :: Int -> Doc -> Doc 
fill width x = best 0 [x]
  where
    best col (d : ds) =
      case d of
        Empty -> best col ds
        Char c -> c Prettify.<> best (col + 1) ds
        Text s -> s Prettify.<> best (col + length s) ds
        Line -> (pad width - col '#') Prettify.<> '\n' Prettify.<> (best 0 ds)
        a `Concat` b -> best col (a : b : ds)
        a `Union` b ->
          nicest
            col
            (best col (a : ds))
            (best col (b : ds))
    best _ _ = Empty

    nicest col a b
      | (width - least) `fits` a = a
      | otherwise = b
      where
        least = min width col
    
    
pad num ch
      | num <= 0 = ""
      | otherwise = ch : pad (num-1) ch

fits :: Int -> String -> Bool
w `fits` _ | w < 0 = False
w `fits` ""        = True
w `fits` ('\n':_)  = True
w `fits` (c:cs)    = (w - 1) `fits` cs
double :: Double -> Doc
double num = text (show num)

char :: Char -> Doc
char c = Char c

(<>) :: Doc -> Doc -> Doc
Empty <> y = y
x <> Empty = x
x <> y = x `Concat` y

hcat :: [Doc] -> Doc
hcat = fold (Prettify.<>)

fold :: (Doc -> Doc -> Doc) -> [Doc] -> Doc
fold f = foldr f empty

fsep :: [Doc] -> Doc
fsep = fold (</>)

(</>) :: Doc -> Doc -> Doc
x </> y = x Prettify.<> softline Prettify.<> y

softline :: Doc
softline = group line

group :: Doc -> Doc
group x = flatten x `Union` x

flatten :: Doc -> Doc
flatten (x `Concat` y) = flatten x `Concat` flatten y
flatten Line = Char ' '
flatten (x `Union` _) = flatten x
flatten other = other

punctuate :: Doc -> [Doc] -> [Doc]
punctuate p [] = []
punctuate p [d] = [d]
punctuate p (d : ds) = (d Prettify.<> p) : punctuate p ds
