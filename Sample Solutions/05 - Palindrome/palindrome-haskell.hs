module Palindrome where

palindrome :: Eq a => [a] -> Bool
palindrome a = a == reverse a
