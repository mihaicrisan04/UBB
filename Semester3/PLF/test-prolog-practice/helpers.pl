
% member
member(X, [X|_]).
member(X, [_|T]) :- member(X, T).

% append
append([], L, L).
append([H|T], L, [H|R]) :- append(T, L, R).

% length
length([], 0).
length([_|T], N) :- length(T, N1), N is N1 + 1.

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
append_number([], N, [N]).
append_number([H|T], N, [H|R]) :- append_number(T, N, R).

% intersection of two lists
intersection([], _, []).
intersection([H|T], L2, [H|R]) :- member(H, L2), intersection(T, L2, R).
intersection([_|T], L2, R) :- intersection(T, L2, R).


% sort a list(Insertion sort)
insert_sorted(E, [], [E]).
insert_sorted(E, [H|T], [E,H|T]) :- E =< H.
insert_sorted(E, [H|T], [H|R]) :- E > H, insert_sorted(E, T, R).

insertion_sort([], []).
insertion_sort([H|T], Sorted) :-
    insertion_sort(T, SortedTail),
    insert_sorted(H, SortedTail, Sorted).


% merge sort
merge_sort([], []).
merge_sort([X], [X]).
merge_sort(List, Sorted) :-
    length(List, Len),
    Len > 1,
    split(List, L1, L2),
    merge_sort(L1, Sorted1),
    merge_sort(L2, Sorted2),
    merge(Sorted1, Sorted2, Sorted).

split([], [], []).
split([X], [X], []).
split([X,Y|Rest], [X|L1], [Y|L2]) :-
    split(Rest, L1, L2).

merge([], L, L).
merge(L, [], L).
merge([X|L1], [Y|L2], [X|L]) :-
    X =< Y,
    merge(L1, [Y|L2], L).
merge([X|L1], [Y|L2], [Y|L]) :-
    X > Y,
    merge([X|L1], L2, L).


% flatten a list
flatten_list([], []).
flatten_list([H|T], FlatList) :-
    is_list(H),
    flatten_list(H, FlatH),
    flatten_list(T, FlatT),
    append(FlatH, FlatT, FlatList).
flatten_list([H|T], [H|FlatT]) :-
    integer(H),
    flatten_list(T, FlatT).

% remove duplicates
remove_duplicates([], []).
remove_duplicates([H|T], [H|R]) :-
    not(member(H, T)),
    remove_duplicates(T, R).
remove_duplicates([H|T], R) :-
    member(H, T),
    remove_duplicates(T, R).

% valley check: a list is a valley if it starts with a decreasing sequence followed by an increasing sequence
valley([], 0).
valley([H|T], V) :- valley([H|T], 0, V).
valley([H|T], V, V) :- length(T, 0).
valley([H1,H2|T], V, R) :-
    H1 > H2,
    V1 is V + 1,
    valley([H2|T], V1, R).
valley([H1,H2|T], V, R) :-
    H1 < H2,
    V1 is V + 1,
    valley([H2|T], V1, R).
valley([H1,H2|T], V, R) :-
    H1 =:= H2,
    valley([H2|T], V, R).

% peak check: a list is a peak if it starts with an increasing sequence followed by a decreasing sequence
peak([], 0).
peak([H|T], P) :- peak([H|T], 0, P).
peak([H|T], P, P) :- length(T, 0).
peak([H1,H2|T], P, R) :-
    H1 < H2,
    P1 is P + 1,
    peak([H2|T], P1, R).
peak([H1,H2|T], P, R) :-
    H1 > H2,
    P1 is P + 1,
    peak([H2|T], P1, R).
peak([H1,H2|T], P, R) :-
    H1 =:= H2,
    peak([H2|T], P, R).
