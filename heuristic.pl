%http://www.cs.huji.ac.il/~ai/projects/2012/SolvingBlocksWorldProblems/

on(a,b).
on(b,c).
on(c,d).

belowInStack(X,Y,S):- 
	block(X),
	block(Y),
	X\=Y,
	ord_memberchk(on(Y,X),S).

belowInStack(X,Y,S):- 
	block(X),
	block(Y),
	X\=Y,
	block(Z),
	Z\=X,
	Z\=Y,
	ord_memberchk(on(Y,Z),S),
	belowInStack(X,Z,S).

mustBeMovedOnce(X,S,G):-
	block(X),
	block(Y), 
	X\=Y,
	block(Z),
	Y\=Z,
	ord_memberchk(on(X,Y),S),
	ord_memberchk(on(X,Z),G).

mustBeMovedOnce(X,S,G):-
	block(X),
	block(Y),
	X\=Y,
	ord_memberchk(on(X,Y),G),
	\+ord_memberchk(on(X,Y),S),
	belowInStack(Y,X,S).



mustBeMovedTwice(X,S,G):-
	block(X),
	block(Y),
	X\=Y,
	ord_memberchk(on(X,Y),S),
	ord_memberchk(on(X,Y),G),
	mustBeMovedOnce(Y,S,G).


mustBeMovedTwice(X,S,G):-
	block(X),
	block(Y),
	X\=Y,
	belowInStack(Y,X,S),
	mustBeMovedTwice(Y,S,G).

%GoalPosition(A) = se la seguenza da A a Table è uguale in S e in G

goalPosition(X,S,G):-
	block(X),
	ord_memberchk(ontable(X),S),
	ord_memberchk(ontable(X),G).

goalPosition(X,S,G):-
	block(X),
	block(Y),
	X\=Y,
	ord_memberchk(on(X,Y),S),
	ord_memberchk(on(X,Y),G),
	goalPosition(Y,S,G).

%mutualPrevention = se sviluppo A e B, sia A che B sono in GoalPosition (nello stato fittizio S* dove ho cambiato A e B rimuovendo predicati e reinserendoli al contrario)
mutualPrevention(X,Y,S,G):-
	block(X),
	block(Y),
	X\=Y,
	swap(S,Pos1,Pos2,SWAPPED)
	goalPosition(X,SWAPPED,G),
	goalPosition(Y,SWAPPED,G).