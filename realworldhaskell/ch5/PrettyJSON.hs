module PrettyJSON 
  (
    renderJValue 
  ) where 

import Numeric (showHex)
import Data.Char (ord)
import Data.Bits (shiftR, (.&.))

import SimpleJSON (JValue(..))
import Prettify (Doc, (<>), char, double, fsep, hcat, punctuate, text,
                 compact, pretty, string)

                 
renderJValue :: JValue  -> Doc
renderJValue (JBool True) = text "true"
renderJValue (JBool False) = text "false"
renderJValue JNull = text "null"
renderJValue (JNumber num) = double num
renderJValue (JString str) = Prettify.string str 
renderJValue (JArray ary) = series '[' ']' renderJValue ary 
renderJValue (JObject obj) = series '{' '}' field obj 
  where field (name, val) = Prettify.string name 
                          Prettify.<> text ": "
                          Prettify.<> renderJValue val 

string :: String -> Doc
-- enclose and hcat defined elsewhere
-- hcat is like concatenate, but for Doc 
string = enclose '"' '"' . hcat . map oneChar

-- wrap a Doc with an opening and closing character 
enclose :: Char -> Char -> Doc -> Doc 
-- <> is a custom function that concatenates Doc objects 
enclose left right x = char left Prettify.<> x Prettify.<> char right 

oneChar :: Char -> Doc 
oneChar c = case lookup c simpleEscapes of 
    Just r -> text r 
    Nothing | mustEscape c -> hexEscape c 
            | otherwise -> char c
  where mustEscape c = c < ' ' || c == '\x7f' || c > '\xff'

simpleEscapes :: [(Char, String)]
simpleEscapes = zipWith ch "\b\n\f\r\t\\\"/" "bnfrt\\\"/"
  where ch a b = (a, ['\\', b])

smallHex :: Int -> Doc 
smallHex x = text "\\u"
         Prettify.<> text (replicate (4 - length h) '0')
         Prettify.<> text h 
      where h = showHex x ""

astral :: Int -> Doc 
astral n = smallHex (a + 0xd800) Prettify.<> smallHex (b + 0xdc00)
-- shiftR shifts bits right
-- .&. does bit-level AND 
  where a = (n `shiftR` 10) .&. 0x3ff 
        b = n .&. 0x3ff 

hexEscape :: Char -> Doc 
hexEscape c | d < 0x10000 = smallHex d 
            | otherwise = astral (d - 0x10000)
  where d = ord c  

series :: Char -> Char -> (a -> Doc) -> [a] -> Doc
series open close item = enclose open close
                       . fsep . punctuate (char ',') . map item
