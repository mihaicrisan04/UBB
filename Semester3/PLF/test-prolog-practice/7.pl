

% a)

contains(X, []):- fail.
contains(X, [X|_]):- !.
contains(X, [_|T]):-
    contains(X, T).

intersection([], _, []).
intersection([H1|T1], L2, [H1|R]):-
    contains(H1, L2),
    intersection(T1, L2, R).
intersection([H1|T1], L2, R):-
    intersection(T1, R).


% Check if an element exists in a list
element_in_list(X, [X|_]). % Base case: X is the head of the list
element_in_list(X, [_|Tail]) :- 
    element_in_list(X, Tail). % Recursive case: check in the tail

% Intersect two lists
intersect([], _, []). % Base case: if the first list is empty, intersection is empty
intersect([Head|Tail], List2, [Head|IntersectTail]) :- 
    element_in_list(Head, List2), % Head is in both lists
    intersect(Tail, List2, IntersectTail). % Recurse with the rest
intersect([_|Tail], List2, IntersectTail) :- 
    intersect(Tail, List2, IntersectTail). % Head is not in List2, skip it



% b)

create_list(M, M, [M]).
create_list(N, M, [N|T]):-
    N =< M,
    NN is N + 1,
    create_list(NN, M, T).



