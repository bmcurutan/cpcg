gcdiv(A,B,S) :-
    gcdRec(A,A,B,S).

gcdRec(G,A,B,S) :-
    B =\= 0 -> gcdRec(B,B,A mod B,S);
    S is A.

lcmul(A,B,S) :-
    Y is A * B,
    gcdiv(A,B,Z),
    S is Y / Z.