module Quicksort where
import Data.List 

quicksort :: Ord a => [a] -> [a]  
quicksort [] = []  
quicksort (x:xs) = quicksort less ++ (x:quicksort rest)    
  where (less, rest) = partition (< x) xs 