expo(A,B,S) :-
    expoRec(1,A,B,S).

expoRec(E,A,B,S) :-
    B =\= 0 -> expoRec(E*A,A,B-1,S);
    S is E.

/*exponent(N, 0, 1).
exponent(N, M, S) :-
    M1 is M-1,
    exponent(N, M1, S1),
    S is N * S1. */