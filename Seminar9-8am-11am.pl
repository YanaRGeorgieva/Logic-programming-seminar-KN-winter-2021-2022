lessOrEqual(X, Y):- X =< Y.

isSorted([]).
isSorted([_]).
isSorted([X, Y|T]):- lessOrEqual(X, Y), isSorted([Y|T]).

psort(L, S):-permutation(L, S), isSorted(S).

%min(L, M)
min([M], M).
min([H|T], M):-min(T, N), min2(N, H, M).

%min([H|T], N):-min(T, N), lessOrEqual(N, H).
%min([H|T], H):-min(T, N), not(lessOrEqual(N, H)).

min2(A, B, A):-lessOrEqual(A, B).
min2(A, B, B):-not(lessOrEqual(A, B)).

remove(X, L, R):- append(A, [X|B], L), append(A, B, R).

% selectionSort(L, S).
selectionSort([], []).
selectionSort([H|T], [M|R]):- min([H|T], M), remove(M, [H|T], Q), selectionSort(Q, R).

% splitByPivot(Pivot, L, SL, LL).
splitByPivot(_, [], [], []).
splitByPivot(Pivot, [H|T], [H|A], B):- splitByPivot(Pivot, T, A, B), lessOrEqual(H, Pivot).
splitByPivot(Pivot, [H|T], A, [H|B]):- splitByPivot(Pivot, T, A, B), not(lessOrEqual(H, Pivot)).

quickSort([], []).
quickSort([Pivot|T], S):- 
    splitByPivot(Pivot, T, A, B), 
    quickSort(A, SA), 
    quickSort(B, SB), 
    append(SA, [Pivot|SB], S).

% HW: bubble sort, merge sort

% [], [LT, Root, RT]
% empty, tree(LT, Root, RT)

% treeSort(L, SortedL).
% makeTree(List, Tree).
% add(X, Tree, NTree).
% leftRootRight(Tree, SortedList).

treeSort(L, SL):- makeTree(L, T), leftRootRight(T, SL).

makeTree([], empty).
makeTree([H|T], Tree):- makeTree(T, Tree1), add(H, Tree1,Tree).

add(Smth, empty, tree(empty, Smth, empty)).
add(Smth, tree(LT, Root, RT), tree(LT1, Root, RT)):- 
    lessOrEqual(Smth, Root),
    add(Smth, LT, LT1).
add(Smth, tree(LT, Root, RT), tree(LT, Root, RT1)):- 
    not(lessOrEqual(Smth, Root)),
    add(Smth, RT, RT1).

leftRootRight(empty, []).
leftRootRight(tree(LT, Root, RT), L):- 
    leftRootRight(LT, LL),
    leftRootRight(RT, RL),
    append(LL, [Root|RL], L).
    

% is, =:= =\=
% < > =< >= 
% + / - * ^ div mod ** // 
% X is 6*7 -> Пресмята се 6*7= 42 -> X = 42

% length(L, N).
length1([], 0).
length1([_|T], N):-length1(T, X), N is X+1.

sum([], 0).
sum([H|T], SumHT):- sum(T, SumT), SumHT is SumT + H.

% nthElement(X, IndX, L).

nthElement(X, 0, [X|_]).
nthElement(X, Counter, [_|T]):- nthElement(X, CounterT, T), Counter is CounterT + 1.
% nthElement(X, Counter, [_|T]):- Counter > 0, Counter1 is Counter - 1, nthElement(X, Counter1, T).

/*
..., -2, -1, 0, 1, 2, ...

0, 1, 2, ...
0, 2, 4, 8, ...
0, 1, 1, 2, 3, 5, 8, 13, ...
Как да генерирате цели числа в даден интервал [A, B], A, B- цели числа?
<0,0>, <0,1>, <1, 0>, ....
?-p(X).
*/

natural(0).
natural(X):-natural(Y), X is Y + 1.

even(0).
even(X):-even(Y), X is Y + 2.

even2(X):-natural(X), X mod 2 =:= 0.

even3(X):-natural(Y), X is Y * 2. 