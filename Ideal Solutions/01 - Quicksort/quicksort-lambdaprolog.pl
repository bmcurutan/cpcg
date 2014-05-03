quicksort([], []).  
quicksort([Head | Tail], Sorted) :-  	
  partition(>(Head), Tail, Less, Rest),  	
  quicksort(Less, SortedLess), 	
  quicksort(Rest, SortedRest), 	
  append(SortedLess, [Head | SortedRest], Sorted). 