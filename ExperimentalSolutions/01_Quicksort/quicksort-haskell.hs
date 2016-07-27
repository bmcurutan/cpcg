module Quicksort where

--temp printed solution
--immutable (copy)
import Data.List (partition)

import Control.Monad.ST
import Data.Array.ST
import Data.Foldable
import Control.Monad

-- convert a list to an array to use as input
qs1 = let items = [5,4,4,3,2,1,-10] in do
      runST $ do
          arr <- newListArray (0,(length items) - 1) items :: ST s (STUArray s Int Int)
          quicksort arr (length items)
          newXs <- getElems arr
          return newXs

qs1a = let items = [] in do
      runST $ do
          arr <- newListArray (0,(length items) - 1) items :: ST s (STUArray s Int Int)
          quicksort arr (length items)
          newXs <- getElems arr
          return newXs

quicksort items len = if (<) len 1
                          then do
                              return()
                          else do 
                              quicksortRec items 0 ((-) len 1)
                             -- items

quicksortRec items leftIndex rightIndex = 
    if (leftIndex >= rightIndex)
        then do
            return()
    else do
        let pivot = leftIndex
        pivot <- partition1 pivot items leftIndex rightIndex
        quicksortRec items leftIndex (pivot - 1)
        quicksortRec items (pivot + 1) rightIndex

--foreachWith xs v f = foldlM (flip f) v xs 

partition1 pivotIndex items leftIndex rightIndex = do
    pivotValue <- readArray items pivotIndex
    swap items pivotIndex rightIndex

    --recursive
    swapIndex <- partRec leftIndex leftIndex rightIndex items pivotValue

    --"iterative"
    {-swapIndex <- foreachWith [leftIndex..rightIndex-1] leftIndex (\i swapIndex -> do
        iVal <- readArray items i
        if (iVal < pivotValue)
            then do
                 swap items i swapIndex
                 return (swapIndex + 1)
            else do
                 return swapIndex )-}

    swap items swapIndex rightIndex
    return swapIndex

partRec swapIndex i rightIndex items pivotValue =
    if (<) i rightIndex
        then do
            do 
                iVal <- (readArray items i)
                if (<) iVal pivotValue
                    then do 
                        swap items i swapIndex
                        partRec (swapIndex+1) (i+1) rightIndex items pivotValue
                    else  
                        partRec swapIndex (i+1) rightIndex items pivotValue
        else do
            return swapIndex


-- helpers
swap items i j = 
    do 
        iVal <- readArray items i
        do 
            jVal <- readArray items j
            writeArray items i jVal
            writeArray items j iVal

-------------------------------------------------------

qs items len = if (<) len 1
               then items
               else do 
                   qsRec items 0 ((-) len 1)

qsRec items leftindex rightindex = if ((>=) leftindex rightindex)
                            then items
                            else do
                                let pivot = (head items) in do
                                     let (less,rest) = partition (< pivot) (tail items) in  
                                    -- let (less,rest) = part pivot (tail items) in do
                                        -- concat [qsRec less 0 ((-) (length less) 1), (pivot:qsRec rest 0 ((-) (length rest) 1))]
                                         qsRec less 0 ((-) (length less) 1) ++ (pivot:qsRec rest 0 ((-) (length rest) 1))
   
part pivot items = 
    partitionRec pivot items

partitionRec pivot items =
    if null items && (>=) (length items) 1
        then do
            let x = head items in do
                let (less,rest) = partitionRec pivot (tail items) in do
                    if (<) x pivot
                        then do
                            (x:less,rest)
                        else do
                            (less,x:rest)
        else do
            ([],[])




-----------------

--quicksort :: Ord a => [a] -> [a] 
{- quicksort items = let len = (length items) in
        if (<) len 1
            then items
            else let pivot = (head items) in do
                     let (less,rest) = partition (< pivot) (tail items) in 
                         let sortedless = quicksort less in
                             let sortedrest = quicksort rest in 
                                 sortedless ++ (pivot:sortedrest) -}

--change pivot to last item in list
{- quicksort1 items = let len = (length items) in
        if (<) len 1
            then items
            else let pivot = (last items) in 
                     let (less,rest) = partition (< pivot) (init items) in 
                         let sortedless = quicksort1 less in
                             let sortedrest = quicksort1 rest in 
                                 sortedless ++ (pivot:sortedrest) -}

{-partition :: Ord a => a -> [a] -> ([a],[a])
partition _ [] = ([],[])
partition p (x:xs) = let (less, rest) = partition p xs in
    if x < p  then (x:less, rest)    
    else (less, x:rest)-}
    

{- partition               :: (a -> Bool) -> [a] -> ([a],[a])
-- # INLINE partition #
partition p xs = foldr (select p) ([],[]) xs

select :: (a -> Bool) -> a -> ([a], [a]) -> ([a], [a])
select p x ~(ts,fs) | p x       = (x:ts,fs)
                    | otherwise = (ts, x:fs) -}

---------------------------------------------------------------------
	
--ideal solution 
{-quicksort [] = []
quicksort (x:xs) = quicksort less ++ (x:quicksort rest)
    where 
--        (less, rest) = partition x xs
		(less, rest) = partition (< x) xs-}

---------------------------------------------------------------------


