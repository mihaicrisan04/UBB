
% Predicate to substitute an element X with Y in a list.
% substitute(+List-list, +X-elem, +Y-elem, -Result-elem)(i, i, i, o)
/*
f(l1, l2, ..., ln, x, y) =  [], n = 0
                            y U f(l2, ..., ln) , l1 = x 
                            l1 U f(l2, ..., ln), l1 != x
*/
substitute([], _, _, []).                  
substitute([X|Tail], X, Y, [Y|NewTail]) :-  
    substitute(Tail, X, Y, NewTail).        
substitute([Head|Tail], X, Y, [Head|NewTail]) :-  
    substitute(Tail, X, Y, NewTail). 


% Predicate to create a sublist from the main list.
% sublist(+List-list, +Start-elem, +End-elem, -Sublist-list)(i, i, i, o)
/*
f(l1, l2, ..., ln, start, end, s) = [], n = 0
                                    f(l2, ..., ln, start - 1, end - 1, s), start > 1
                                    l1 U f(l2, ..., ln, start - 1, end - 1, s), start = 1
*/
sublist(List, Start, End, Sublist) :- 
    nth1(Start, List, _, Rest),          % Drop the elements before the start index.
    EndIndex is End - Start + 1,         % Calculate the length of the sublist.
    prefix(Sublist, Rest),               % Take the first EndIndex elements.
    length(Sublist, EndIndex).           % Ensure the sublist is of the correct length.

nth1(1, [H|T], H, T).                    % Base case, first element is H.
nth1(N, [H|T], E, [H|Rest]) :-           % Recursive case, count down.
    N > 1,
    N1 is N - 1,
    nth1(N1, T, E, Rest).

prefix([], _).                           % Base case, empty list is a prefix of any list.
prefix([H|T], [H|Rest]) :-               % Recursive case, match head of both lists.
    prefix(T, Rest).


