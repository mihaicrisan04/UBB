
% a)

/* merge_sort(l1, l2, ..., ln)  = [] , n = 0
                                = [l1] , n = 1
                                = merge(merge_sort(l1, ..., li), merge_sort(li+1, ..., ln)) , n > 1
                                where li = n / 2

merge_sort(List-list, Sorted-list) (i, o)
*/
merge_sort([], []).
merge_sort([X], [X]).
merge_sort(List, Sorted) :-
    length(List, Len),
    Len > 1,
    split(List, L1, L2),
    merge_sort(L1, Sorted1),
    merge_sort(L2, Sorted2),
    merge(Sorted1, Sorted2, Sorted).

/* 
split(l1, l2, ..., ln) = [], n = 0
                       = l1, n = 1
                       = ([l1, l2, ..., li], [li+1, ..., ln], []) , n > 1 where li = n / 2
split(List-list, List-list, List-list) (i, o, o)
*/
split([], [], []).
split([X], [X], []).
split([X,Y|Rest], [X|L1], [Y|L2]) :-
    split(Rest, L1, L2).

/*
merge(l1, l2, .., ln, m1, m2, .., mn) = [] , n = 0, m = 0
                                      = [l1] + merge(l2, .., ln, m1, m2, .., mn) , l1 <= m1
                                      = [m1] + merge(l1, l2, .., ln, m2, .., mn) , l1 > m1
                                      = [l1, l2, ..., ln], n > 0, m = 0
                                      = [m1, m2, ..., mn], n = 0, m > 0
merge(L1, L2, Result) (i, i, o)
 */
merge([], L, L).
merge(L, [], L).
merge([X|L1], [Y|L2], [X|L]) :-
    X =< Y,
    merge(L1, [Y|L2], L).
merge([X|L1], [Y|L2], [Y|L]) :-
    X > Y,
    merge([X|L1], L2, L).


% b)
% For a heterogeneous list, formed from integers numbers and lists of digits, merge all the sublists with removing the double values.
% [1, [2, 31, 4, 5, [1, 4, 6], 3, [1, 3, 7, 9, 101, 5, [1, 1, 11], 81]]] => [1, 2, 3, 4, 6, 7, 9, 10, 11].


remove_duplicates([], []).
remove_duplicates([H|T], [H|T1]) :-
    not(member(H, T)),
    remove_duplicates(T, T1).
remove_duplicates([H|T], T1) :-
    member(H, T),
    remove_duplicates(T, T1).

flatten_list([], []).
flatten_list([H|T], FlatList) :-
    is_list(H),
    flatten_list(H, FlatH),
    flatten_list(T, FlatT),
    append(FlatH, FlatT, FlatList).
flatten_list([H|T], [H|FlatT]) :-
    integer(H),
    flatten_list(T, FlatT).

/* het_sort(l1, l2, ..., ln) = [] , n = 0
                            = merge_sort(remove_duplicates(flatten_list(l1, l2, ..., ln))) , n > 0
het_sort(List-list, Sorted-list) (i, o)
*/
het_sort(List, Sorted) :-
    flatten_list(List, FlatList),
    remove_duplicates(FlatList, NoDupList),
    merge_sort(NoDupList, Sorted).

append([], L, L).
append([H|T], L, [H|R]) :-
    append(T, L, R).


% call functions
% het_sort([1, [2, 31, 4, 5, [1, 4, 6], 3, [1, 3, 7, 9, 101, 5, [1, 1, 11], 81]]], Sorted).
% merge_sort([1, 2, 31, 4, 5, 1, 4, 6, 3, 1, 3, 7, 9, 101, 5, 1, 1, 11, 81], Sorted).


