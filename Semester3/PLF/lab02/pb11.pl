
% Predicate to substitute an element X with Y in a list.
% substitute(+List, +X, +Y, -Result)
substitute([], _, _, []).                  % Base case: empty list, nothing to substitute.
substitute([X|Tail], X, Y, [Y|NewTail]) :-  % If head matches X, replace it with Y.
    substitute(Tail, X, Y, NewTail).        % Recurse on the rest of the list.
substitute([Head|Tail], X, Y, [Head|NewTail]) :-  % If head does not match, keep it.
    substitute(Tail, X, Y, NewTail).            % Recurse on the rest of the list.


% Predicate to create a sublist from the main list.
% sublist(+List, +Start, +End, -Sublist)
sublist(List, Start, End, Sublist) :- 
    nth1(Start, List, _, Rest),          % Drop the elements before the start index.
    EndIndex is End - Start + 1,         % Calculate the length of the sublist.
    prefix(Sublist, Rest),               % Take the first EndIndex elements.
    length(Sublist, EndIndex).           % Ensure the sublist is of the correct length.

% Helper predicate: nth1/4 finds the element at the Start index and returns the remaining list.
nth1(1, [H|T], H, T).                    % Base case, first element is H.
nth1(N, [H|T], E, [H|Rest]) :-           % Recursive case, count down.
    N > 1,
    N1 is N - 1,
    nth1(N1, T, E, Rest).

% Helper predicate: prefix/2 gets a prefix of a list.
prefix([], _).                           % Base case, empty list is a prefix of any list.
prefix([H|T], [H|Rest]) :-               % Recursive case, match head of both lists.
    prefix(T, Rest).


