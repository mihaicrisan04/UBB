

% a)
% Write a predicate to determine if a list has even numbers of elements without counting the elements from
% the list.

even_length([]).
even_length([_, _|T]):-
    even_length(T).
% even_length([_]):- % not needed because the predicate will fail if it can't find any clause that matches the current goal
    % fail.


% b) Write a predicate to delete first occurrence of the minimum number from a list.

% minimum(X, Y, Min) succeeds if Min is the smaller of X and Y.
minimum(X, Y, X) :- X =< Y.
minimum(X, Y, Y) :- X > Y.

min_list([H], H).
min_list([H|T], R):-
    min_list(T, RR),
    minimum(RR, H, R).

remove_elem([], E, []).
remove_elem([E|T], E, T):-
    !.
remove_elem([H|T], E, [H|R]):-
    remove_elem(T, E, R).

rm_min(L, R):-
    min_list(L, Min),
    remove_elem(L, Min, R).




