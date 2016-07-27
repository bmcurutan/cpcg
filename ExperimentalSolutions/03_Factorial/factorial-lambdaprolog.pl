factorial(I,S) :-
    facRec(1,I,S).

facRec(F,I,S) :-
    I > 0 -> facRec(F*I,I-1,S);
    S is F.




/*factorial(0, 1).
factorial(N, FacN) :-
    N1 is N - 1,
    factorial(N1, FacN1),
    FacN is N * FacN1.*/
