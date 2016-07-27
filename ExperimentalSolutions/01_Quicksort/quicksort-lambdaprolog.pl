%use_module(library(apply)).
quicksort(Items,Len,Squicksort) :- 
    Len < 1->Squicksort = Items;
    quicksortRec(Items,0,Len-1,Squicksort).

quicksortRec(Items,LeftIndex,RightIndex,SquicksortRec) :-
    LeftIndex >= RightIndex -> SquicksortRec = Items;
    [Head|Tail] = Items, Pivot is last([Head|Tail]),
    partition(>(Pivot), init[Head|Tail], Less, Rest),
    length(Less,LenLess),
    length(Rest,LenRest),
    quicksortRec(Less,0, LenLess-1, SortedLess),
    quicksortRec(Rest, 0,LenRest-1, SortedRest),
    append(SortedLess, [Pivot|SortedRest], SquicksortRec). 

/*quicksort([], []).  

quicksort([H | T], S) :-  	
  partition(>(H), T, L, R),  	
  quicksort(L, SL), 	
  quicksort(R, SR), 	
  append(SL, [H | SR], S).   

last([X],S):- S is X.
last([Y|Tail],S):- last(Tail,S).*/

    
test :- write([100,24,-2,0,29,98,-18,201,24]), 
quicksort([100,24,-2,0,29,98,-18,201,24], 9,Sorted), write(Sorted). 

