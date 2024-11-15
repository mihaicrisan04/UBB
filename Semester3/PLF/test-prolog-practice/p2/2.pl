


% a)
% Sort a list with keeping double values in resulted list. E.g.: [4 2 6 2 3 4] --> [2 2 3 4 4 6]

% insert_sorted(Elem, List, Result) inserts Elem into the sorted List to produce Result.
insert_sorted(E, [], [E]).
insert_sorted(E, [H|T], [E,H|T]) :- E =< H.
insert_sorted(E, [H|T], [H|R]) :- E > H, insert_sorted(E, T, R).

% insertion_sort(List, Sorted) sorts List using insertion sort to produce Sorted.
insertion_sort([], []).
insertion_sort([H|T], Sorted) :-
    insertion_sort(T, SortedTail),
    insert_sorted(H, SortedTail, Sorted).

sort_list(L, SortedWithDuplicates) :-
    insertion_sort(L, SortedWithDuplicates).

% b)
% For a heterogeneous list, formed from integer numbers and list of numbers, write a predicate to sort every
% sublist, keeping the doubles.
% Eg.: [1, 2, [4, 1, 4], 3, 6, [7, 10, 1, 3, 9], 5, [1, 1, 1], 7] =>
% [1, 2, [1, 4, 4], 3, 6, [1, 3, 7, 9, 10], 5, [1, 1, 1], 7].


sort_sublists([], []).
sort_sublists([H|T], [H1|R]) :-
    is_list(H),
    sort_list(H, H1),
    sort_sublists(T, R).
sort_sublists([H|T], [H|R]) :-
    not(is_list(H)),
    sort_sublists(T, R).
