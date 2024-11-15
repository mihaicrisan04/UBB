
% a)
% Write a predicate to determine the difference of two sets.

diff([], _, []).
diff([H|T1], S2, [H|R]):-
    not(member(S2, H)),
    diff(T1, S2, R).
diff([H|T1], S2, R):-
    member(S2, H),
    diff(T1, S2, R).

% diff([1, 2, 3], [1, 2, 4], R).


% b)
% Write a predicate to add 1 after all the elements from a list

add_one([], []).
add_one([H|T], [H, 1|R]):-
    add_one(T, R).