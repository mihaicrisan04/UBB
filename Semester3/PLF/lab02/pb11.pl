
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

% substitute([1, 2, 3, 2, 4], 2, 9, Result).


% Predicate to create a sublist from the main list.
% sublist(+List-list, +Start-elem, +End-elem, -Sublist-list)(i, i, i, o)
/*
f(l1, l2, ..., ln, start, end, s) = [], start > end
                                    f(l2, ..., ln, start - 1, end - 1, s), start > 0
                                    l1 U f(l2, ..., ln, 0, end - 1, s), end >= 0
*/
sublist(_, Start, End, []) :- 
    Start > End.

sublist([_|T], Start, End, Result) :- 
    Start > 0, 
    NewStart is Start - 1, 
    NewEnd is End - 1, 
    sublist(T, NewStart, NewEnd, Result).

sublist([H|T], 0, End, [H|Result]) :- 
    End >= 0, 
    NewEnd is End - 1, 
    sublist(T, 0, NewEnd, Result).

% sublist([1,2,3,4,5,6],1,3,R).

