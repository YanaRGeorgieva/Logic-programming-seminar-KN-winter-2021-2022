
% flatten(L, R) [[[[[],a]], b], [c]] -> [a, b, c]
isList([]).
isList([_|_]).

flatten1([], []).
flatten1([H|T], R):- flatten1(H, FH), flatten1(T, FT), append(FH, FT, R).
flatten1(X, [X]):- not(isList(X)).
myFlatten(L, R):- isList(L), flatten1(L, R).

% G=<V,E> 
% V=[a,b,...]
% E=[[a,b], ....]

edge([_, E], X, Y):- member([X, Y], E); member([Y, X], E).
% General scheme for traversing in depth
% dfs(X):-goal(X).
% dfs(X):- edge(X, Y), dfs(Y).

path(_, B, B, Visited, Path):- reverse([B|Visited], Path).
path([V, E], A, B, Visited, Path):- edge([V, E], A, C),  not(member(C, Visited)),
    path([V, E], C, B, [A|Visited], Path).

path([V, E], A, B, Path):- path([V, E], A, B, [], Path).

% a -> b -> c -> d
%         c -> a

graph([[a, b, c, d], [[a, b], [c, a], [b, c], [c, d]]]).
graph([[a, b, c, d], [[a, b], [c, a], [b, c]]]).


% hasCycle(G, P)
hasCycle([V, E], [X|Path]):- edge([V, E], X, Y), path([V, E], Y, X, Path), length(Path, N), N > 2.

% isConnected([[X|V], E])
isConnected([[X|V], E]):- not(( member(Y, V), not( path([V, E], X, Y, _) ) )).

remove(X, L, R):- append(A, [X|B], L), append(A, B, R).

/*
  1. Реализирайте алгоритъм stree(V, E, ST), генериращ в ST покриващото дърво на графа G(V, E)
*/
% spanningTree([V, E], ST).
spanningTree([V, E], ST):- V = [X|Rest], spanningTree1([V, E], [X], Rest, ST).
spanningTree1(_, _, [], []).
spanningTree1([V, E], Visited, NotVisited, [[X, Y]|ST]):- 
    member(X, Visited), remove(Y, NotVisited, NewNotVisited), 
    edge([V, E], X, Y), spanningTree1([V, E], [Y|Visited], NewNotVisited, ST).

isConnected2(G):- spanningTree(G, _).

isTree(T):- T = [_, E], spanningTree(T, ST), length(E, M), length(ST, M).


/*2. Септемврийска сесия 2013 г.
	Зад. 1. Нека G е неориентиран граф. Множеството от вър-
	ховете на G е представено със списък V от върховете, всяко
	ребро v е представено с двуелементен списък на краища-
	та му, а множеството от ребрата на G е представено със
	списък E от ребрата.
	Да се дефинира на Пролог предикат
	а) con(V, E), който разпознава дали представеният с V и
	E граф е свързан.
	б) crit(V, E, X), който по дадени V и E на свързан граф ге-
	нерира в X списък на всички върхове, чието отстраняване
	води до граф, който не е свързан. (3 + 3 точки)
	3. 12.04.2014 г.
	Зад. 1. a) Да се дефинира на Пролог предикат
	p(X,A,B), който по даден списък от двойки X =
	[[a1, b1],[a2 ,b2 ],...,[an , bn ]] проверява дали шахматният
	кон може да се придвижи с един ход от поле с коорди-
	нати [A,B] на поле, чиито координати не са елемент на
	списъка X.
	б) Да се дефинира предикат q(X), който проверява да-
	ли шахматният кон може да се придвижи от поле с
	координати [1,1] на поле [8,8], без да преминава през
	полета, чиито координати са елемент на списъка X =
	[[a1, b1],[a2 ,b2 ],...,[an , bn ]].
	Забележка. Шахматната дъска е с размер 8 × 8.
*/
criticalVertex([V, E], X):- remove(X, V, NewV), removeEdges(E, X, NewE), not(isConnected([NewV, NewE])).

removeEdges([], _, []).
removeEdges([H|T], X, [H|R]):- not(member(X, H)), removeEdges(T, X, R).
removeEdges([H|T], X, R):- member(X, H), removeEdges(T, X, R).

% a -b -c
%   -c -d

/*
Зад. 1. Диаметър на списък наричаме разликата между броя срещания на най-често срещан
елемент на списъка и броя срещания на най-рядко срещан елемент на списъка. Да се дефинира
на Пролог едноместен предикат p, който по даден списък от списъци L разпознава дали:
всички елементи на L имат един и същ диаметър.
*/
count(_, [], 0).
count(X, [X|T], N):- count(X, T, M), N is M + 1.
count(X, [H|T], N):- X \= H, count(X, T, N).

maxCount(X, L, N):- count(X, L, N), not((member(Y, L), count(Y, L, M), M > N)).
minCount(X, L, N):- count(X, L, N), not((member(Y, L), count(Y, L, M), M < N)).

diameter(L, D):- member(X, L), maxCount(X, L, Max), member(Y, L), minCount(Y, L, Min), D is Max - Min.

p([]).
p(L):- remove(X, L, L1), diameter(X, DX), not(( member(Y, L1), diameter(Y, DY), DX =\= DY )).
/*Зад. 2. Списък от три числа [X,Y,R] ще интерпрети-
раме като окръжност с център hX,Y i и радиус R. 
a) Да се дефинира генератор circles1(X,Y,R,Z,T,S), който по
дадена окръжност [X,Y,R] при преудовлетворяване ге-
нерира в Z, T и S окръжностите, които съдържат ок-
ръжността [X,Y,R].
b) Да се дефинира генератор circles2(X,Y,R,Z,T,S), който по
дадена окръжност [X,Y,R] при преудовлетворяване ге-
нерира в Z, T и S окръжностите, които се съдържат ок-
ръжността [X,Y,R].
*/

/*
24 януари 2018 изпита
Фенски списък е краен списък, всеки елемент на който е някоя от буквите 1, 2 или е
фенски списък, като никои два съседни елемента не са еднакви букви.
Да се дефинира на Пролог едноместен предикат p(X), който при преудовлетворяване
генерира в X всички фенски списъци, които се записват на Пролог с краен брой “[”
*/


