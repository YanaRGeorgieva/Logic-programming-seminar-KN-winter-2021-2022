/*
Some links:
    https://www.swi-prolog.org/
    http://www.learnprolognow.org/lpnpage.php?pageid=top
    https://www.ic.unicamp.br/~meidanis/courses/mc336/2009s2/prolog/problemas/
    http://cs.union.edu/~striegnk/courses/esslli04prolog/index.php
    http://kti.ms.mff.cuni.cz/~bartak/prolog.old/learning.html
*/
/* Променливи: Maria, X, Y, This_lalalalalallala, _, _jsajka
Константи: атоми и числа:
атом: 'нещо такова', "lalal", this_atom, a, +-*>; спец: [], {}, ;, !
числа: integer, floating point
Съставни термове: foo(h(0),baz(0)), foo(a)
Списъци (специални термове): [], (X-element, Y-list -> [X|Y])
[a,[], [a,b]]
Съждителни връзки: (, -> LogicAnd); (; -> LogicOr); (not(unary) -> LogicNot).
not((p1 , p2))
Предикати: започват с малки букви p(X, Y), lessEq(X, Y).
CLAUSES: FACTS, RULES, GOALS

p(T1, T2, ..., Tn).
p(T1, T2, ..., Tn):-p1(...), p2(), ... pk().
?-p1(), ..., pk().

p(1, 2).
parent(maria, ivan).

q(X, 1).
q(X, Z):-p(Y).

q(X, Z):- Z=1; p(Y).

?-q(2, 3).

maria 
  |
ivan
 |     |
petar  hristo
|
georgy
*/
parent(maria, ivan).
parent(ivan, petar).
parent(peter, georgi).
parent(ivan, hristo).

grandparent(X, Y):-parent(X, Z), parent(Z, Y).

% Y ->Z .... Z-> X 
% Y -> X

successor(X, Y):-parent(Y, X).
successor(X, Y):-parent(Y, Z), successor(X, Z).

d(x, 1).
d(X, 0):-number(X).
d(X+Y, DX+DY):-d(X, DX), d(Y, DY).
d(X*Y, DX*Y+X*DY):-d(X, DX), d(Y, DY).
d(sin(X), cos(X)*DX):-d(X, DX).
% X = Y : unify X and Y

/* [], [a,c,a,as,[]]
[H|T]
[a, c, a] = [a|[c|[a|[]]]]
list(H, T), empty
*/

% member(X, Y): X е елемент на списъка Y
member(X, [X|_]).
member(X, [_|T]):- member(X, T).

first(First, [First|_]).
% second(Second, [_, Second|_]).
% second(Second, [_|[Second|[]]]).
second(Second, [_|T]):- first(Second, T).

% last(Last, List).
last(Last, [Last|[]]). % last(Last, [Last]).
last(Last, [_|T]):- last(Last, T).

preLast(PreLast, [_|T]):- preLast(PreLast, T).
preLast(PreLast, [PreLast, _]).

% append(A, B, List) A=[a,a,3], B=[[], a,1], List=[a,a,3,[],a,1]

% [A1|A] B [A1|List] List=append(A, B)
% [] B B

append([], B, B).
append([A1|A], B, [A1|List]):-append(A, B, List).

member1(X, L):- append(_, [X|_], L).

last1(X, L):- append(_, [X], L).

prefix(Prefix, List):- append(Prefix, _, List).
suffix(Suffix, List):- append(_, Suffix, List).
infix(Infix, List):- prefix(X, List), suffix(Infix, X).
% infix(Infix, List):- suffix(X, List), prefix(Infix, X).
infix1(Infix, List):- append(X, Infix, Something), append(Something, Y, List). % BADDDDD

% remove(X, List, ResultList).
remove(X, List, ResultList):- append(A, [X|B], List), append(A, B, ResultList).

remove1(X, [X|T], T).
remove1(X, [H|T], [H|ResultList]):- remove1(X, T, ResultList).

insert(X, List, ResultList):- append(A, B, List), append(A, [X|B], ResultList).

insert1(X, List, ResultList):- remove1(X, ResultList, List).

% HW removeAll(X, List, RList). reverse, permutation




