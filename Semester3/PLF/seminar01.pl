% Multiply the elements of a list with a constant value.
% [1, 2, 3], k=10 => [10, 20, 30]
% mulk(L-list, K-number, R-result list)
% flow model (i, i, o)
mulk([], _, []).
mulk([H|T], K, R):-
    H1 is H*K,
    mulk(T, K, TR),
    R = [H1|TR].

% Appen an element at the end of a list
% [1, 2, 3], e=11 => [1, 2, 3, 11]
appnd([], E, [E]).
appnd([H|T], E, [H|TR]):-
    appnd(T, E, TR).
    
% Compute the sum of elements of a numerical list
sumlist([], 0).
sumlist([H|T], S):-
    sumlist(T, TS),
	S is H+TS.

% Compute the product of even elements in a numerical list
% productlist(L-list, P-number)
% flow model (i, 0) (i, i)
prodlist([], 1).
prodlist([H|T], P):-
    H mod 2 =\= 0,
    prodlist(T, P).
prodlist([H|T], P):-
    H mod 2 =:= 0,
    prodlist(T, TP),
    P is H*TP.

% Given a lsit of numbers, remove all increasing sequences
% [1,2,4,6,5,7,8,2,1] => [2,1]
removeinc([], []).
removeinc([H|T], R):-
    


