:- use_module(library(clpfd)).

% Генериране на списъци с четна дължина?
evenLengthLists([]).
evenLengthLists([_, _|T]):- evenLengthLists(T).

natural(0).
natural(N):- natural(M), N is M + 1.

evenLengthLists1(L):- natural(N), N mod 2 =:= 0, length(L, N).

lengthCLP([], N):- N #= 0.
lengthCLP([_|T], N):- N #> 0, lengthCLP(T, N - 1).

evenLengthListsCLP(L):- lengthCLP(L, 2 * _).

% [0,1,2,3,4,5,6,7]- пермутация на редовете-> 1 ред = 1 царица разположена на нея; долян ляв ъгъл е началото на дъската

queens(N, L):- generateQueenList(N, L), goodPlacement(L).

goodPlacement(L):- goodPlacement(L, 0).
goodPlacement([], _).
goodPlacement([Q|RQ], N):- N1 is N + 1, notInBattle(Q, RQ, N, N1), goodPlacement(RQ, N1).

notInBattle(_, [], _, _).
notInBattle(RowQ, [RowQ1|RestQ], ColumnQ, ColumnQ1):- 
    RowQ - ColumnQ =\= RowQ1 - ColumnQ1, 
    ColumnQ + RowQ =\= ColumnQ1 + RowQ1, 
    ColumnQ11 is ColumnQ1 + 1, notInBattle(RowQ, RestQ, ColumnQ, ColumnQ11).

select(L, X, R):- append(A, [X|B], L), append(A, B, R).

permutation([], []).
permutation(L, [X|R]):- select(L, X, L1), permutation(L1, R).

range(B1, B, []):- B1 > B.
range(A, B, [A|R]):- A =< B, A1 is A + 1, range(A1, B, R).

generateQueenList(N, L):- N1 is N - 1, range(0, N1, L1), permutation(L1, L).


queensCLP(N, L):- generateQueenListCLP(N, L), goodPlacementCLP(L), label(L).

goodPlacementCLP(L):- goodPlacementCLP(L, 0).
goodPlacementCLP([], _).
goodPlacementCLP([Q|RQ], N):- N1 #= N + 1, notInBattleCLP(Q, RQ, N, N1), goodPlacementCLP(RQ, N1).

notInBattleCLP(_, [], _, _).
notInBattleCLP(RowQ, [RowQ1|RestQ], ColumnQ, ColumnQ1):- 
    RowQ - ColumnQ #\= RowQ1 - ColumnQ1, 
    ColumnQ + RowQ #\= ColumnQ1 + RowQ1, 
    ColumnQ11 #= ColumnQ1 + 1, notInBattleCLP(RowQ, RestQ, ColumnQ, ColumnQ11).

generateQueenListCLP(N, L):- Nm1 #= N - 1, lengthCLP(L, N), L ins 0..Nm1, all_distinct(L).


node(g1, 1).
node(g1, 2).
node(g1, 3).
node(g1, 4).
node(g1, 5).
node(g1, 6).
node(g1, 7).
node(g1, 8).
node(g1, 9).
node(g1, 10).
node(g1, 11).

node((VV, _), V) :-
    member(V, VV).


edge(g1, 1, 2).
edge(g1, 1, 3).
edge(g1, 1, 4).
edge(g1, 1, 5).
edge(g1, 1, 6).

edge(g1, 2, 8).
edge(g1, 2, 1).
edge(g1, 2, 11).

edge(g1, 3, 9).
edge(g1, 3, 1).
edge(g1, 3, 7).

edge(g1, 4, 8).
edge(g1, 4, 10).
edge(g1, 4, 1).

edge(g1, 5, 1).
edge(g1, 5, 9).
edge(g1, 5, 11).

edge(g1, 6, 7).
edge(g1, 6, 1).
edge(g1, 6, 10).

edge(g1, 7, 8).
edge(g1, 7, 3).
edge(g1, 7, 6).
edge(g1, 7, 11).

edge(g1, 8, 9).
edge(g1, 8, 4).
edge(g1, 8, 2).
edge(g1, 8, 7).

edge(g1, 9, 8).
edge(g1, 9, 10).
edge(g1, 9, 5).
edge(g1, 9, 3).

edge(g1, 10, 6).
edge(g1, 10, 4).
edge(g1, 10, 9).
edge(g1, 10, 11).

edge(g1, 11, 7).
edge(g1, 11, 2).
edge(g1, 11, 5).
edge(g1, 11, 10).

edge((_, EE), V1, V2) :-
    member((V1, V2), EE).


:-table anyPath(+, +, +).
anyPath(G, V, V):- node(G, V).
anyPath(G, V, U):- edge(G, V, W), anyPath(G, W, U).

:-table generatePath(+, +, +, -).
generatePath(G, V, V, [V]):- node(G, V).
generatePath(G, V, U, [V|R]):- edge(G, V, W), generatePath(G, W, U, R).

% :-table fibCLP(+, +). Not good to mix table with clpfd
% fibCLP(0, 0).
% fibCLP(1, 1).
% fibCLP(N, Z):-N #> 1, Z #=X+Y, Nm1 #= N - 1, Nm2 #= N - 2, fibCLP(Nm1, X), fibCLP(Nm2, Y), label([N, Z]).

:-table fib(+, +).
fib(0, 0).
fib(1, 1).
fib(N, Z):- N > 1, Nm1 is N - 1, Nm2 is N - 2, fib(Nm1, X), fib(Nm2, Y), Z is X +Y.

lazyRange(A, B, R):- bagof(X, between(A, B, X), R).

% a:-b, c, !, d, e.
% a:-f, g.

not(A):-call(A), !, fail.
not(A).

min2(A, A, B):-A =< B, !.
min2(B, A, B).

