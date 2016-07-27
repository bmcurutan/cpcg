factorial(0,1).
factorial(N,S) :- 
  N>0,
  N1 is N-1,
  factorial(N1,S1),
  S is N*S1.

