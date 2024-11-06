
% remove_odd(L, R) - R is the list obtained from L by removing all the odd numbers
remove_odd([], []).
remove_odd([H|T], [H|T1]) :- 0 is H mod 2, remove_odd(T, T1).
remove_odd([H|T], T1) :- 1 is H mod 2, remove_odd(T, T1).

% process(L, R) - R is the list obtained from L by removing all the odd numbers
process(L, R) :- remove_odd(L, R).


m_mountain([_]).
m_mountain([H1, H2|T]) :- H1 < H2, m_valley([H2|T]).
m_mountain([H1, H2|T]) :- H1 > H2, m_mountain([H2|T]).

m_valley([_]).
m_valley([H1, H2|T]) :- H1 > H2, m_mountain([H2|T]).
m_valley([H1, H2|T]) :- H1 < H2, m_valley([H2|T]).

m_mountain_valley(L) :- m_mountain(L).
m_mountain_valley(L) :- m_valley(L).

