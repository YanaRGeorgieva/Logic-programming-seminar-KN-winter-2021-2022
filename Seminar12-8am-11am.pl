

/*Зад. 2. Списък от три числа [X,Y,R] ще интерпретираме 
като окръжност с център hX,Y i и радиус R. 
a) Да се дефинира генератор circles1(X,Y,R,Z,T,S), който по
дадена окръжност [X,Y,R] при преудовлетворяване генерира в 
Z, T и S окръжностите, които съдържат окръжността [X,Y,R].
b) Да се дефинира генератор circles2(X,Y,R,Z,T,S), който по
дадена окръжност [X,Y,R] при преудовлетворяване генерира в Z, T и S 
окръжностите, които се съдържат в окръжността [X,Y,R].
*/
inPoint(X, Y, R, X1, Y1):- 
    (X-X1)*(X-X1) + (Y-Y1)*(Y-Y1) =< R * R.
inCircle(X, Y, R, X1, Y1, R1):- 
    X1pR1 is X1 + R1, X1mR1 is X1 - R1,
    Y1pR1 is Y1 + R1, Y1mR1 is Y1 - R1,
    not((
        between(X1mR1, X1pR1, Z),
        between(Y1mR1, Y1pR1, T),
        inPoint(X1, Y1, R1, Z, T),
        not(inPoint(X, Y, R, Z, T))
    )).

circles2(X,Y,R,Z,T,S):- 
    between(1, R, S), 
    XpR is X + R, XmR is X - R,
    YpR is Y + R, YmR is Y - R,
    between(XmR, XpR, Z),
    between(YmR, YpR, T),   
    inCircle(X, Y, R, Z, T, S).

genKS(0, 0, []).
genKS(K, S, [XI|R]):- K > 0, K1 is K - 1, 
    between(0, S, XI), S1 is S - XI, genKS(K1, S1, R).

nat(0).
nat(N):- nat(M), N is M + 1.

circle1(X, Y, R, Z, T, S):- nat(N), genKS(3, N, [Z, T, S]), inCircle(Z, T, S, X, Y, R).

/*
24 януари 2018 изпита
Фенски списък е краен списък, всеки елемент на който е някоя от буквите 1, 2 или е
фенски списък, като никои два съседни елемента не са еднакви букви.
Да се дефинира на Пролог едноместен предикат p(X), който при преудовлетворяване
генерира в X всички фенски списъци, които се записват на Пролог с краен брой “[”
*/
condition([]).
condition(X):- not(is_list(X)), 
    member(X, [1, 2]).
condition(L):- is_list(L), 
    not(( append(_, [X, X|_], L), member(X, [1, 2]) )), 
    not(( member(T, L), not(condition(T)) )).

p(X):- nat(N), tree(N, X), condition(X). 
tree(0, []).
tree(N, [CurrentBranch|L]):-
    N > 0,
    between(1, N, VerticesCurrentBranch), 
    LeftVertices is N - VerticesCurrentBranch,
    generateBranch(VerticesCurrentBranch, CurrentBranch),
    tree(LeftVertices, L).

generateBranch(1, X):- member(X, [1, 2, []]).
generateBranch(N, T):- N > 1, N1 is N - 1, tree(N1, T). 

p1(X):- nat(N), tree1(N, X), is_list(X), condition(X). 

tree1(1, X):- member(X, [1, 2, []]).
tree1(N, X):- N > 1, N1 is N - 1, sums(N1, Sum), hTree1(Sum, X).

sums(0, []).
sums(N, [XI|R]):- between(1, N, XI), N1 is N - XI, sums(N1, R).

hTree1([], []).
hTree1([H|T], [X|R]):- tree1(H, X), hTree1(T, R).

% Супер Марио е подложен на поредното предизвикателство. Той се намира на
% надморска височина N единици и пред него е зейнала огромна пропаст, в която
% стърчат метални кулички с различни надморски височини, подредени в редич-
% ка плътно една след друга стигаща до другия край на пропастта. До него има
% ръчка, която при всяко дърпане преподрежда куличките в нова редичка.
% За улеснение нека си представим редицата от куличките като списък, чиито
% елементи са списъци от крайни вложения на празния списък
% ( [[[[[]]]], [[]], [[[[[[]]]]]]] −→ списък с началните разположения на
% кулички с надморски височини с 3, 1 и 5 единици).
% Той може да скача и пропада на височина максимум 2 единици.
% Да се дефинира предикат futureBride(Towers, N, L), който по дадено пред-
% ставяне на редица от кулички Towers и първоначална надморска височина N
% генерира в L чрез преудовлетворяване всички последователности на куличките,
% така че Супер Марио да може да стигне от единия край на пропастта до другия
% и да спаси принцесата от чудовището.

:- use_module(library(clpfd)). % -> q / r
% constraint logic programming over finite domains
/* https://www.swi-prolog.org/man/clpfd.html
http://www.pathwayslms.com/swipltuts/clpfd/clpfd.html
https://www.metalevel.at/prolog/clpz
https://github.com/triska/clpz
https://www.swi-prolog.org/pldoc/man?section=summary-lib-clpfd */

% #/\/2	P and Q hold.
% #</2	The arithmetic expression X is less than Y.
% #<==/2	Q implies P.
% #<==>/2	P and Q are equivalent.
% #=/2	The arithmetic expression X equals Y.
% #=</2	The arithmetic expression X is less than or equal to Y.
% #==>/2	P implies Q.
% #>/2	Same as Y #< X.
% #>=/2	Same as Y #=< X.
% #\/1	Q does _not_ hold.
% #\/2	Either P holds or Q holds, but not both.
% #\//2	P or Q holds.
% #\=/2	The arithmetic expressions X and Y evaluate to distinct integers.
% all_different/1	Like all_distinct/1, but with weaker propagation.
% all_distinct/1	True iff Vars are pairwise distinct.
% automaton/3	Describes a list of finite domain variables with a finite automaton.
% automaton/8	Describes a list of finite domain variables with a finite automaton.
% chain/2	Zs form a chain with respect to Relation.
% circuit/1	True iff the list Vs of finite domain variables induces a Hamiltonian circuit.
% cumulative/1	Equivalent to cumulative(Tasks, [limit(1)]).
% cumulative/2	Schedule with a limited resource.
% disjoint2/1	True iff Rectangles are not overlapping.
% element/3	The N-th element of the list of finite domain variables Vs is V.
% fd_dom/2	Dom is the current domain (see in/2) of Var.
% fd_inf/2	Inf is the infimum of the current domain of Var.
% fd_size/2	Reflect the current size of a domain.
% fd_sup/2	Sup is the supremum of the current domain of Var.
% fd_var/1	True iff Var is a CLP(FD) variable.
% global_cardinality/2	Global Cardinality constraint.
% global_cardinality/3	Global Cardinality constraint.
% in/2	Var is an element of Domain.
% indomain/1	Bind Var to all feasible values of its domain on backtracking.
% ins/2	The variables in the list Vars are elements of Domain.
% label/1	Equivalent to labeling([], Vars).
% labeling/2	Assign a value to each variable in Vars.
% lex_chain/1	Lists are lexicographically non-decreasing.
% scalar_product/4	True iff the scalar product of Cs and Vs is in relation Rel to Expr.
% serialized/2	Describes a set of non-overlapping tasks.
% sum/3	The sum of elements of the list Vars is in relation Rel to Expr.
% tuples_in/2	True iff all Tuples are elements of Relation.
% zcompare/3	Analogous to compare/3, with finite domain variables A and B.

/* 
X in A..B -> singleton=5 / 3..7/ inf/number..sup/number / Dom1 \/ Dom2 -> 2..3 \/ 14..90
label([X]).
[H|T] ins A..B същото е като първото, но за списък [H|T]
all_distinct(L)
 */

lengthO([], 0).
lengthO([_|T], N):- lengthO(T, M), N is M + 1.


lengthCLP([], N):-N #= 0.
lengthCLP([_|T], N):-N #= M+1, lengthCLP(T, M).

betweenCLP(A, B, X):-X in A..B, label([X]).

sumCLP(N, A, B):-A+B #= N, A in 0..N, label([A, B]).

% fact(N, F)
fact(N, F):-N#=0, F#=1.
fact(N, F):-N#>0, F#=F1*N, fact(N-1, F1).

permCLP(P):-lengthCLP(P, 5), P ins 0..4, all_distinct(P), label(P).





