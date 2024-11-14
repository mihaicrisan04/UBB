

% a)
% Write a predicate to remove all occurrences of a certain atom from a list

% remove_occurrences(L: List, E: Atom, R: List)
remove_occurrences([], _, []).
remove_occurrences([H|T], E, R):-
    H = E, % check if H is the same atom as E
    remove_occurrences(T, E, R).
remove_occurrences([H|T], E, [H|R]):-
    H \= E,
    remove_occurrences(T, E, R).

% remove_occurrences([1, 2, 3, 4, 5, 3], 3, R).


% b)
% Define a predicate to produce a list of pairs (atom n) from an initial list of atoms. 
% In this initial list atom has n occurrences
% Eg.: numberatom([1, 2, 1, 2, 1, 3, 1], X) => X = [[1, 4], [2, 2], [3, 1]].

% numberatom(L: List, R: List)

numberatom([], []).
numberatom([H|T], R):-


% numberatom([1, 2, 1, 2, 1, 3, 1], R).
