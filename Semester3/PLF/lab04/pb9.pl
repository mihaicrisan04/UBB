
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
    exists_with_diff_1(Vi, L, I),
    I1 is I + 1,
    condition(L, I1).

% Helper predicate to check if there exists an element on the left with an absolute difference of 1
exists_with_diff_1(Vi, L, I) :-
    I1 is I - 1,
    between(1, I1, J),
    nth1(J, L, Vj),
    abs(Vi - Vj) =:= 1,
    !.


permute(N, R):-
    numlist(1, N, L),
    % findall(RP, perm(L, RP), R).
    findall(RP, (perm(L, RP), condition(RP)), R).
	
% findall(R, perm([1,2,3,4], R), X)