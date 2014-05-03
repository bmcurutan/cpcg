:- module(palindrome,[palindrome/2]).
palindrome(A,S) :-
    reverse(A,B),
    A = B.

