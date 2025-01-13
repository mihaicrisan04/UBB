% Write a prolog program to generate all the subsets of N elements from a list. Explain the proposed solution.
% For example for the list [2,3,4] and N = 2, the program should generate all the subsets of 2 elements from the list.
% [2,3], [2,4], [3,4]


% Base case: If the number of elements to select is 0, the subset is the empty list
subset([], 0, []).

% If we have more elements to select than available in the list, fail
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
arrangement(_, 0, []).

% Recursive case: Select an element and arrange the remaining N-1 elements
arrangement(List, N, [H|R]) :-
    N > 0,
    N1 is N - 1,
    select(H, List, Rest),
    arrangement(Rest, N1, R).

cond([], 1).
cond([H|T], P) :- 
    cond(T, P1),
    P is P1 * H.

arr(L, N, P, R) :-
    findall(X, (arrangement(L, N, X), cond(X, P)), R).

% ------------------------------------------------------------------------------------------------

% Base case: The permutation of an empty list is an empty list
permutation([], []).

% Recursive case:
% Select an element H from the list, generate permutations of the remaining list, and prepend H
permutation([H|T], Perm) :-
    permutation(T, PermT),
    pick(H, Perm, PermT).

% Helper predicate to pick an element H from List, resulting in Rest
pick(H, [H|T], T).
pick(H, [X|T], [X|Rest]) :-
    pick(H, T, Rest).

cond([_]).
cond([X,Y|T]) :-
    Abs is abs(X-Y),
    Abs < 4,
    cond([Y|T]).

permutations_all(L, R) :-
    findall(X, (permutation(L, X), cond(X)), R).


% ------------------------------------------------------------------------------------------------

f([], []).
f([H|T], [H|R]) :- f(T, R).
f([H|T], R) :- H mod 2 =:= 0, f(T, R).

% ------------------------------------------------------------------------------------------------

