module Exponent where

expo a b = exponentRec 1 a b

exponentRec e a b = 
    if b /= 0
        then do exponentRec (e*a) a (b-1)
        else do e

{-expo n 0 = 1
expo n m = n * expo n (m-1)-}
