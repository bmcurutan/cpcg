module GCDLCM where

--greatest common divisor
gcdiv a b = gcdRec a a b

gcdRec g a b = 
    if b /= 0
        then do gcdRec b b ((mod) a b)
        else do a

--lowest common multiple
lcmul a b = let y = a * b in
                let z = gcdiv a b in
                    div y z


