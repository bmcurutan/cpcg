gcdiv(A,B,S) :- A = B, S = A.
gcdiv(A,B,S) :- 
  A < B,
  B1 is B mod A,
  (B1 = 0 -> S = A; gcdiv(A, B1, S)).
gcdiv(A,B,S) :- A > B, gcdiv(B,A,S).

lcmul(A,B,S) :-
    gcdiv(A,B,S1),
    S is (A*B) / S1.