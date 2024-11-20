

% insertEl(E, L, R) - R is the list L with element E inserted at the beginning
/*
insertEl(e, l1, l2, ..., ln) = e U l1l2...ln, n >= 0
                               l1 U insertEl(e, l2, ..., ln), n > 0
*/
insertEl(E, L, [E|L]).
insertEl(E, [H|T], [H|R]) :- insertEl(E, T, R).



% subseq(L, R) - R is a subsequence of L
subset([], []).
subset([H|T], [H|R]) :- subset(T, R).
subset([_|T], R) :- subset(T, R).

% perm(L, R)
perm([], []).
perm(L, [H|R]) :- 
    insertEl(H, T, L),
    perm(T, R).

% comb(L, K, R)
comb(_, 0, []).
comb([H|T], K, [H|R]) :- 
    K > 0,
    K1 is K - 1,
    comb(T, K1, R).
comb([_|T], K, R) :-
    K > 0,
    comb(T, K, R).

% arr(L, K, R) 
arr(L, K, R) :- 
    comb(L, K, R1),
    perm(R1, R).


% -----------------------------------

findPerm(L, R) :-
    findall(RP, perm(L, RP), R).    

findComb(L, K, R) :-
    findall(RP, comb(L, K, RP), R).

findArr(L, K, R) :-
    findall(RP, arr(L, K, RP), R).

findSubset(L, R) :-
    findall(RP, subset(L, RP), R).


% -----------------------------------

% all subsequences of a list that are in form of a valley
% sol1: generate all permutations of the list and check if they are valleyg

mainValley([]).
mainValley([_]).
mainValley([H1, H2|T]) :- 
    H1 > H2,
    decreasing(T).
mainValley([H1, H2|T]) :-
    H1 < H2,
    increasing(T).

decreasing([]).
decreasing([_]).
decreasing([H1, H2|T]) :- 
    H1 > H2,
    decreasing([H2|T]).
decreasing([H1, H2|T]) :-
    H1 =< H2,
    false.

increasing([]).
increasing([_]).
increasing([H1, H2|T]) :- 
    H1 < H2,
    increasing([H2|T]).
increasing([H1, H2|T]) :-
    H1 >= H2,
    false.

onesol(L, P) :-
    subset(L, SS),
    perm(SS, P),
    mainValley(P).  

findOneValley(L, R) :-
    findall(RP, onesol(L, RP), R).  

% sol2: generate all the solution already in the form of valley


