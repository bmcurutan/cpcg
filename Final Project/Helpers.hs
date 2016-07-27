module Helpers where

--External modules
import Text.PrettyPrint
import Data.Char

--This module stores defined helper documentation to easily pretty print code

--basics
dot, ques, hash, coldash, pipe, dblslash, backslash, dbldash :: Doc
dot = text "."
ques = text "?"
hash = text "#"
coldash = text ":-"
pipe = text "|"
dblslash = text "//"
dbldash = text "--"
backslash = text "\\"
unit = brackets empty
amp = text "&"

alligs :: Doc -> Doc
alligs x = text "<" <> x <> text ">" -- wrap in < >

--operations
less, greater, minus, plus, times, divide, modu, eqequals, notequal, lessequal, notless, nott, andd, orr :: Doc
less = text "<"
greater = text ">"
minus = text "-"
plus = text "+"
times = text "*"
divide = text "/"
modu = text "%"
eqequals = text "=="
notequal = text "!="
lessequal = text "<="
notless = text ">="
nott = text "!"
conc = text "++"
andd = text "&&"
orr = text "||"

--duplicates
star, slash, dash :: Doc
star = text "*" --same as times
slash = text "/" --same as divide
dash = text "-" --same as minus

--conjunctives
(<^>),(<||>),(<|>),(<->),(<:>) :: Doc -> Doc -> Doc
x <^> y = x <> comma <> y
x <||> y = x <> text "\t|\t" <> y
x <|> y = x <> text "|" <> y
x <-> y = x <> text "->" <> y
x <:> y = x <> colon <> y

--format strings
upcase, lowcase :: [Char] -> Doc
upcase (c:cs) = text $ toUpper c:cs --capitalize first letter of string
lowcase (c:cs) = text $ toLower c:cs --make first letter lowercase

