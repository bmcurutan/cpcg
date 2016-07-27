:- module(maximum,[maxi/3]).
maxi(A,B,S) :-
    S is max(A,B).

/*maxi(N,M,S) :-
    N > M -> S is N;
       S is M.*/
    
