
% a)
% Write a predicate to determine the lowest common multiple of a list formed from integer numbers

% lowest_common_multiple(L: List, R: Number)
lcm(L, R):-
    lcm(L, 1, R).

lcm([], R, R).
lcm([H|T], Acc, R):-
    lcm(H, Acc, Acc1),
    lcm(T, Acc1, R).

lcm(A, B, R):-
    R is A * B // gcd(A, B).

% lcm([2, 3, 4], R).


% b) Write a predicate to add a value v after 1-st, 2-nd, 4-th, 8-th, … element in a list.

% add_value(L: List, V: Number, R: List)
add_value(L, V, R):-
    add_value(L, V, 1, 1, R).

add_value([], _, _, _, []).
add_value([H|T], V, I, P, [H, V|R]):-
    I =:= P, % I == P
    P1 is P * 2,
    I1 is I + 1,
    add_value(T, V, I1, P1, R).
add_value([H|T], V, I, P, [H|R]):-
    I =\= P, % I != P
    I1 is I + 1,
    add_value(T, V, I1, P, R).

% add_value([1, 2, 3, 4, 5], 0, R).


