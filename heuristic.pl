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


replaceOn(X,Y,S,SPP):-
	block(Z),
	X\=Z,
	ord_memberchk(on(X,Z), S),!,
	ord_del_element(S, on(X,Z), DLS),
	ord_add_element(DLS, on(Y,Z), SPP).
 
replaceOnTable(X,Y,S,SPP):-
	ord_memberchk(ontable(X), S),!,
	ord_del_element(S, ontable(X), DLS),
	ord_add_element(DLS, ontable(Y), SPP).

replaceBlock(X,Y,S,SP):-
	replaceOnTable(X,Y,S,SP),!.

replaceBlock(X,Y,S,SP):-
	replaceOn(X,Y,S,SP).	

swapPreventionState(X,Y,S,FAKE_S):-
	X\=Y,
	ord_memberchk(on(X,Y), S),!,
	ord_del_element(S, on(X,Y), DLS),
	ord_add_element(DLS, on(Y,X), TEMP_S),
	block(Z),
	X\=Z,
	Y\=Z,
	ord_memberchk(on(Y,Z), TEMP_S),
	ord_del_element(TEMP_S, on(Y,Z), DLSS),
	ord_add_element(DLSS, on(X,Z), FAKE_S).

swapPreventionState(X,Y,S,FAKE_S):-
	X\=Y,	
	ord_memberchk(on(Y,X), S),!,
	ord_del_element(S, on(Y,X), DLS),
	ord_add_element(DLS, on(X,Y), TEMP_S),
	block(Z),
	X\=Z,
	Y\=Z,
	ord_memberchk(on(X,Z), TEMP_S),
	ord_del_element(TEMP_S, on(X,Z), DLSS),
	ord_add_element(DLSS, on(Y,Z), FAKE_S).


swapPreventionState(X,Y,S,FAKE_S):-
	replaceBlock(X,Y,S,SP),
	replaceBlock(Y,X,S,SPP),
	ord_union(SP, SPP, FAKE_S).

%mutualPrevention = se sviluppo A e B, sia A che B sono in GoalPosition (nello stato fittizio S* dove ho cambiato A e B rimuovendo predicati e reinserendoli al contrario)
mutualPrevention(X,Y,S,G):-
	block(X),
	block(Y),
	X\=Y,
	swapPreventionState(X,Y,S,FAKE_S),
	goalPosition(X,FAKE_S,G),
	goalPosition(Y,FAKE_S,G).