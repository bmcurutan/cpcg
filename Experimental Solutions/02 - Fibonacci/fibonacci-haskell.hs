module Fibonacci where

fibonacci n = 
    if n < 2
        then do n
        else do fibonacciRec n 0 1 2 n 

fibonacciRec f n1 n2 i n = 
    if (<=) i n
        then do fibonacciRec (n1+n2) n2 (n1+n2) (i+1) n
        else do f

{-fibonacci :: (Num a, Num a1) => a -> a1
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)-}