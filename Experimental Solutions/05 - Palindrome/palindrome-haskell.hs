module Palindrome where

{-palindrome s = let x = s in 
                   if s == reverse s
                       then True
                       else False-}

--palindrome x = x == reverse x

palindrome :: Eq a => [a] -> Bool
palindrome a = let x = a in do
        let b = reverse a in
            a == b
