% lisp 5 l1

% Generate all permutation of N (N - given) respecting the property: for every 2<=i<=n exists an 1<=j<=i,
% so |v[i]-v[j]|=1. 


% insertEl(e, l1...ln) = e U l1...ln, n >= 0
%						l1 U insertEl(e, l2...ln), n > 0
% insertEl(E - elem, L - list, R - list) (i,i,o)
insertEl(E, L, [E|L]).
insertEl(E, [H|T], [H|R]):-
	insertEl(E, T, R).
	

% perm(l1l2...ln) = [], if n = 0
% 					insertEl(l1, perm(l2...ln)), n > 0
perm([],[]).
perm([H|T], R):-
	perm(T, P),
	insertEl(H, P, R).


condition(L) :-
    condition(L, 2).

% Base case: If the index exceeds the length of the list, the condition is satisfied
condition(L, I) :-
    length(L, Len),
    I > Len.

% Recursive case: Check if there exists an element on the left with an absolute difference of 1
condition(L, I) :-
    nth1(I, L, Vi),
    J is I - 1,
    exists_with_diff_1(Vi, L, I, J),
    I1 is I + 1,
    condition(L, I1).

% Helper predicate to check if there exists an element on the left with an absolute difference of 1
exists_with_diff_1(Vi, L, I, J) :-
    J > 0,
    nth1(J, L, Vj),
    abs(Vi - Vj) =:= 1.
exists_with_diff_1(Vi, L, I, J) :-
    J > 0,
    J1 is J - 1,
    exists_with_diff_1(Vi, L, I, J1).

nth1(1, [H|_], H).
nth1(N, [_|T], R):-
    N1 is N - 1,
    nth1(N1, T, R).

permute(N, R):-
    numlist(1, N, L),
    % findall(RP, perm(L, RP), R).
    findall(RP, (perm(L, RP), condition(RP)), R).
	
% findall(R, perm([1,2,3,4], R), X)