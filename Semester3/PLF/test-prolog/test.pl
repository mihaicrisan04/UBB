

% member
member(X, [X|_]).
member(X, [_|T]):- 
    member(X, T).


% remove duplicates
/*
remove_duplicates(l1, l2, ..., ln) = [] , n = 0
                                   = l1 + remove_duplicates(l2, ..., ln) , l1 not in l2, ..., ln
                                   = remove_duplicates(l2, ..., ln) , l1 in l2, ..., ln
remove_duplicates(List-list, List-list) (i, o)
*/
remove_duplicates([], []).
remove_duplicates([H|T], [H|R]):-
    not(member(H, T)),
    remove_duplicates(T, R).
remove_duplicates([H|T], R):-
    member(H, T),
    remove_duplicates(T, R).


% merge 2 lists
/*
merge(l1, l2, .., ln, m1, m2, .., mn) = [] , n = 0, m = 0
                                      = l1 + merge(l2, .., ln, m1, m2, .., mn) , l1 <= m1
                                      = m1 + merge(l1, l2, .., ln, m2, .., mn) , l1 > m1
                                      = [l1, l2, ..., ln], n > 0, m = 0
                                      = [m1, m2, ..., mn], n = 0, m > 0
merge(L1-list, L2-list, Result-list) (i, i, o)
 */
merge([], L, L).
merge(L, [], L).
merge([X|L1], [Y|L2], [X|L]):-
    X =< Y,
    merge(L1, [Y|L2], L).
merge([X|L1], [Y|L2], [Y|L]):-
    X > Y,
    merge([X|L1], L2, L).


% merge 2 lists and remove duplicates
merge_list(L1, L2, R):-
    merge(L1, L2, L),
    remove_duplicates(L, R).