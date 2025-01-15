% Write a prolog program to generate all the subsets of N elements from a list. Explain the proposed solution.
% For example for the list [2,3,4] and N = 2, the program should generate all the subsets of 2 elements from the list.
% [2,3], [2,4], [3,4]


% Base case: If the number of elements to select is 0, the subset is the empty list
subset([], 0, []).

% If we have more elements to select than available in the list, fail
% not mandatory, but can be used to avoid unnecessary backtracking
subset(List, N, _) :- 
    length(List, L), 
    N > L, 
    !,
    fail.

% Recursive case: Either select the head element or skip it
subset([H|T], N, [H|R]) :-  % Include head element
    N > 0,
    N1 is N - 1,
    subset(T, N1, R).
subset([_|T], N, R) :-      % Skip head element
    subset(T, N, R).

% Helper predicate to generate all subsets of N elements from a list
subset_all(L, N, R) :-
    findall(X, subset(L, N, X), R).

% Example usage
% ?- subset_all([2,3,4], 2, R).
% R = [[2,3], [2,4], [3,4]].



% ------------------------------------------------------------------------------------------------


% Predicate to generate all arrangements (permutations) of N elements from a list

% Base case: Selecting 0 elements results in the empty arrangement
my_select(X, [X|T], T).
my_select(X, [H|T], [H|Rest]) :-
    my_select(X, T, Rest).

arr(_, 0, []).

% Recursive case: Select an element and arrange the remaining N-1 elements
arr(List, N, [H|R]) :-
    N > 0,
    N1 is N - 1,
    my_select(H, List, Rest),
    arr(Rest, N1, R).

cond([], 1).
cond([H|T], P) :- 
    cond(T, P1),
    P is P1 * H.

arr_all(L, N, P, R) :-
    findall(X, (arr(L, N, X), cond(X, P)), R).

% ------------------------------------------------------------------------------------------------

% Helper predicate to pick an element H from List, resulting in Rest; this is the same as my_select
pick(X, [X|T], T).
pick(X, [H|T], [X|Rest]) :-
    pick(X, T, Rest).

% Base case: The permutation of an empty list is an empty list
perm([], []).

% Recursive case:
% Select an element H from the list, generate perms of the remaining list, and prepend H
perm([H|T], Perm) :-
    perm(T, PermT),
    pick(H, Perm, PermT).

cond([_]).
cond([X,Y|T]) :-
    Abs is abs(X-Y),
    Abs < 4,
    cond([Y|T]).

perms_all(L, R) :-
    findall(X, (perm(L, X), cond(X)), R).


% ------------------------------------------------------------------------------------------------

comb(_, 0, []).

% % not mandatory, but can be used to avoid unnecessary backtracking
comb(List, N, _) :- 
    length(List, L), 
    N > L, 
    !,
    fail.

comb([H|T], N, [H|R]) :-
    N > 0,
    N1 is N - 1,
    comb(T, N1, R).
comb([_|T], N, R) :-
    comb(T, N, R).

comb_all(L, N, R) :-
    findall(X, comb(L, N, X), R).

% ------------------------------------------------------------------------------------------------

f([], []).
f([H|T], [H|R]) :- f(T, R).
f([H|T], R) :- H mod 2 =:= 0, f(T, R).

% ------------------------------------------------------------------------------------------------

