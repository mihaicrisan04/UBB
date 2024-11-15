

% a)
% Insert an element on the position n in a list 

insert(L, E, P, R):-
    length(L, Len),
    P < Len,
    insert(L, E, 0, P, R).

insert([H|T], E, I, I, [H, E|T]).
insert([H|T], E, I, P, [H|R]):-
    II is I + 1,
    insert(T, E, II, P, R).

% b)
% . Define a predicate to determine the greatest common divisor of all numbers from a list. 

gcd_list([X], X).
gcd_list([H|T], R):-
    gcd_list(T, G),
    R is gcd(H, G).

