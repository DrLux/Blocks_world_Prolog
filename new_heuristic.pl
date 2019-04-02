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
	ord_memberchk(ontable(X),G),
	ord_memberchk(ontable(X),S).

goalPosition(X,S,G):-
	block(X),
	block(Y),
	X\=Y,
	ord_memberchk(on(X,Y),G),
	ord_memberchk(on(X,Y),S),
	goalPosition(Y,S,G).


%mutualPrevention = se sviluppo A e B, sia A che B sono in GoalPosition (nello stato fittizio S* dove ho cambiato A e B rimuovendo predicati e reinserendoli al contrario)
mutualPrevention(X,Y,S,G):-
	block(X),
	block(Y),
	X\=Y,
	swapPreventionState(X,Y,S,FAKE_S),
	goalPosition(X,FAKE_S,G),
	goalPosition(Y,FAKE_S,G).

swapPreventionState(X,Y,S,FAKE_S):-
	replaceBlock(X,Y,S,SP),
	replaceBlock(Y,X,S,SPP),
	ord_union(SP, SPP, FAKE_S).

replaceBlock(X,Y,S,SP):-
	replaceAbove(X,Y,S,SA),
	replaceBelow(X,Y,S,SB),
	ord_union(SA,SB,FAKE_S),
	replaceOnTable(X,Y,FAKE_S,SP).


replaceAbove(X,Y,S,SPP):- %nel caso in cui abbia qualcosa diverso da Y sopra di X
	block(Z),
	x\=Z,
	ord_memberchk(on(Z,X),S),
	Y\=Z,!, 
	ord_del_element(S,on(Z,X),SP),
	ord_add_element(SP,on(Z,Y),SPP).

replaceAbove(X,Y,S,SPP):- %nel caso in cui abbia Y sopra X
	ord_memberchk(on(Y,X),S),!,
	ord_del_element(S,on(Y,X),SP),
	ord_add_element(SP,on(X,Y),SPP).


replaceAbove(_,_,S,S). % nel caso in cui non ci sia nulla sopra X

replaceBelow(X,Y,S,SPP):- %nel caso in cui abbia qualcosa diverso da Y sotto di X
	block(Z),
	x\=Z,
	ord_memberchk(on(X,Z),S),
	Y\=Z,!, 
	ord_del_element(S,on(X,Z),SP),
	ord_add_element(SP,on(Y,Z),SPP).

replaceBelow(X,Y,S,SPP):- %nel caso in cui abbia Y sotto X
	ord_memberchk(on(X,Y),S),!,
	ord_del_element(S,on(X,Y),SP),
	ord_add_element(SP,on(Y,X),SPP).


replaceBelow(_,_,S,S). % nel caso in cui non ci sia nulla sotto X

replaceOnTable(X,Y,S,ST):-
	ord_memberchk(ontable(X),S),!,
	ord_del_element(S,ontable(X),SP),
	ord_add_element(SP,ontable(Y),ST).

replaceOnTable(_,_,S,S).

