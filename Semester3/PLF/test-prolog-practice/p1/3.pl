
% a)
% Define a predicate to remove from a list all repetitive elements.
% Eg.: l=[1,2,1,4,1,3,4] => l=[2,3]) 

% remove_repetitive(L: List, R: List)
remove_repetitive([], []).
remove_repetitive([H|T], [H|R]):-
    remove_occurrences(T, H, R1),
    remove_repetitive(R1, R).

remove_occurrences([], _, []).
remove_occurrences([E|T], E, R):-
    remove_occurrences(T, E, R).
remove_occurrences([H|T], E, [H|R]):-
    remove_occurrences(T, E, R).

% remove_repetitive([1, 2, 1, 4, 1, 3, 4], R).


% b)
% Remove all occurrence of a maximum value from a list on integer numbers

% remove_max(L: List, R: List)
remove_max([], []).
remove_max(L, R):-
    max_list(L, M),
    remove_occurrences(L, M, R).

max_list([E], E).
max_list([H|T], M):-
    max_list(T, M1),
    M is max(H, M1).

% remove_max([1, 2, 3, 4, 5, 4], R).
