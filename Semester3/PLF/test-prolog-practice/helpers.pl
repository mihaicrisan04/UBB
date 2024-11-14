
member(X, [X|_]).
member(X, [_|T]) :- member(X, T).

append([], L, L).
append([H|T], L, [H|R]) :- append(T, L, R).

length([], 0).
length([_|T], N) :- length(T, N1), N is N1 + 1.

findall(X, Goal, List) :-
    bagof(X, Goal, List), !.
findall(_, _, []).

% reverse a number

% reverse a list

% append a number to a list

% is list?

% member

% sublist

% intersection

