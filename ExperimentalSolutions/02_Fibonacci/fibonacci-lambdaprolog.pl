fibonacci(N,S) :-
    N < 2 -> S is N;
    fibRec(N,0,1,2,N,S).

fibRec(F,N1,N2,I,N,S) :-
    I =< N -> fibRec(N1+N2,N2,N1+N2,I+1,N,S);
    S is F.
       

test :- fibonacci(0, S0), write(S0), fibonacci(1, S1), write(S1), fibonacci(2, S2), write(S2), fibonacci(3, S3), write(S3), fibonacci(4, S4), write(S4), fibonacci(5, S5), write(S5). 

/*fibonacci(0, S) :- 
    S is 0.
fibonacci(1, S) :-
    S is 1.
fibonacci(N, S) :-
    A is N - 1, 
    fibonacci(A, N1),
    B is N - 2,
    fibonacci(B, N2),
    S is N1 + N2.*/