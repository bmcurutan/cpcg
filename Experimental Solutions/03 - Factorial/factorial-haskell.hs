module Factorial where

factorial i = 
    factorialRec 1 i
    
factorialRec f i =
    if i > 0
        then do factorialRec (f*i) (i-1)  
        else f 

  
  