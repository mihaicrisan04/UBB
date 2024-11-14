
% member
member(X, [X|_]).
member(X, [_|T]) :- member(X, T).

% append
append([], L, L).
append([H|T], L, [H|R]) :- append(T, L, R).

% length
length([], 0).
length([_|T], N) :- length(T, N1), N is N1 + 1.

% reverse
findall(X, Goal, List) :-
    bagof(X, Goal, List), !.
findall(_, _, []).

% reverse a number
reverse_number(N, R) :- reverse_number(N, 0, R).
reverse_number(0, R, R).
reverse_number(N, Acc, R) :- 
    N > 0,
    N1 is N // 10,
    Acc1 is Acc * 10 + N mod 10,
    reverse_number(N1, Acc1, R).

% reverse a list
reverse_list(L, R) :- reverse_list(L, [], R).
reverse_list([], R, R).
reverse_list([H|T], Acc, R) :- reverse_list(T, [H|Acc], R).

% append a number to a list
append_number(L, N, R) :- reverse_number(N, RN), append(L, [RN], R).

% intersection
intersection([], _, []).
intersection([H|T], L2, [H|R]) :- member(H, L2), intersection(T, L2, R).
intersection([_|T], L2, R) :- intersection(T, L2, R).


