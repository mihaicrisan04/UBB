/*
suma(l1, l2, ..., ln) = 0, n = 0
                        l1 + suma(l2, ..., ln), n > 0
*/
% suma(+Lista, -Suma) (i, o)
suma([], 0).
suma([H|T], S) :- 
    suma(T, S1),
    S is S1 + H.


/*
sumc(l1, l2, ..., ln, c) = 0, n = 0
                              sumc(l2, ..., ln, c + l1), n > 0
*/
% sumc(+Lista, +Contor, -Suma) (i, i, o)
sumc([], C, C).
sumc([H|T], C, S) :- 
    C1 is C + H,
    sumc(T, C1, S).

mainSumc(L, S) :- sumc(L, 0, S).

/*
occurrences(l1, l2, ..., ln, e) = 0, n = 0
                                  1 + occurrences(l2, ..., ln, e), l1 = e
                                  occurrences(l2, ..., ln, e), l1 != e
*/
% occurrences(+Lista, +Element, -Numar) (i, i, o)
occurrences([], _, 0).
occurrences([H|T], E, R) :-
    H =:= E,
    occurrences(T, E, R1),
    R is R1 + 1.
occurrences([H|T], E, R) :-
    H =\= E,
    occurrences(T, E, R).



/*
nOcc(l1, l2, ..., ln, e) = 0, n = 0
                           1 + nOcc(l2, ..., ln, e), l1 = e
                           nOcc(l2, ..., ln, e), l1 != e
*/
% nOcc(+Lista, +Element, -Numar) (i, i, o)
nOcc([], _, C, C).
nOcc([E|T], E, C, R) :-
    C1 is C + 1,
    nOcc(T, E, C1, R).
nOcc([H|T], E, C, R) :-
    H =\= E,
    nOcc(T, E, C, R).

mainNOC(L, E, R) :- nOcc(L, E, 0, R).



% Remove from a list elements that appear only once
% removeOnce(+Lista, -Rezultat) (i, o)
removeOnce([], []).
removeOnce([H|T], R) :-
    nOcc([H|T], H, 0, NOcc),
    NOcc =:= 1,
    removeOnce(T, R).
removeOnce([H|T], [H|R]) :-
    nOcc([H|T], H, 0, NOcc),
    NOcc > 1,
    removeOnce(T, R).



% Compute the gcd of all elems in a lsit
% gcdList(+Lista, -Rezultat) (i, o)
gcdList([], 0).
gcdList([H|T], R) :-
    gcdList(T, R1),
    R is gcd(H, R1).

% gcd(+A, +B, -Rezultat) (i, i, o)
gcd(A, 0, A).
gcd(A, B, R) :- 
    B =\= 0,
    A1 is A mod B,
    gcd(B, A1, R).


