% Mondo dei blocchi
% Specifica delle azioni mediante precondizioni ed effetti alla STRIPS
% Gli stati sono rappresentati con insiemi ordinati


applicable(putOnBlock(X,Y),S):-
	block(X),
	block(Y),
	X\=Y,
	ord_memberchk(clear(X),S), %forse qui basta il member 
	ord_memberchk(clear(Y),S).

applicable(putOnPillar(X,P),S):-
	block(X),
	pillar(P),
	ord_memberchk(clear(X),S),
	ord_memberchk(clear(P),S).
	

%se X era su un altro cubo	
transform(putOnBlock(X,Y),S1,S2):-
	member(on(X,Z),S1), %se X era su un altro blocco
	X\=Z, !,
	list_to_ord_set([on(X,Z),clear(Y)],DLS),
	ord_subtract(S1,DLS,S),
	list_to_ord_set([on(X,Y),clear(Z)],ALS),
	ord_union(S,ALS,S2).


%se X era su un pilastro
transform(putOnBlock(X,Y),S1,S2):-
	member(onpillar(X,P),S1), %se X era su un altro blocco
	list_to_ord_set([onpillar(X,P),clear(Y)],DLS),
	ord_subtract(S1,DLS,S),
	list_to_ord_set([on(X,Y),clear(P)],ALS),
	ord_union(S,ALS,S2).

%se X era su un altro blocco	
transform(putOnPillar(X,P),S1,S2):-
	member(on(X,Z),S1),!, %se X era su un altro blocco
	X\=Z,!,
	list_to_ord_set([on(X,Z),clear(P)],DLS),
	ord_subtract(S1,DLS,S),
	list_to_ord_set([onpillar(X,P),clear(Z)],ALS),
	ord_union(S,ALS,S2).

%se X era su un pilastro
transform(putOnPillar(X,P),S1,S2):-
	member(onpillar(X,R),S1),
	list_to_ord_set([onpillar(X,R),clear(P)],DLS),
	ord_subtract(S1,DLS,S),
	list_to_ord_set([onpillar(X,P),clear(R)],ALS),
	ord_union(S,ALS,S2).
	

%% VECCHIO MODELLO
/*
applicable(pickup(X),S):-
	block(X),
	ord_memberchk(ontable(X),S),
	ord_memberchk(clear(X),S),
	ord_memberchk(handempty,S).
	
applicable(putdown(X),S):-
	block(X),
	ord_memberchk(holding(X),S).
	
applicable(stack(X,Y),S):-
	block(X), block(Y), X\=Y,
	ord_memberchk(holding(X),S),
	ord_memberchk(clear(Y),S).

applicable(unstack(X,Y),S):-
	block(X), block(Y), X\=Y,
	ord_memberchk(on(X,Y),S),
	ord_memberchk(clear(X),S),
	ord_memberchk(handempty,S).
	
transform(pickup(X),S1,S2):-
	block(X),
	list_to_ord_set([ontable(X),clear(X),handempty],DLS),
	ord_subtract(S1,DLS,S),
	list_to_ord_set([holding(X)],ALS),
	ord_union(S,ALS,S2).
	
transform(putdown(X),S1,S2):-
	block(X),
	list_to_ord_set([holding(X)],DLS),
	ord_subtract(S1,DLS,S),
	list_to_ord_set([ontable(X),clear(X),handempty],ALS),
	ord_union(S,ALS,S2).

transform(stack(X,Y),S1,S2):-
	block(X), block(Y), X\=Y,
	list_to_ord_set([holding(X),clear(Y)],DLS),
	ord_subtract(S1,DLS,S),
	list_to_ord_set([on(X,Y),clear(X),handempty],ALS),
	ord_union(S,ALS,S2).

transform(unstack(X,Y),S1,S2):-
	block(X), block(Y), X\=Y,
	list_to_ord_set([on(X,Y),clear(X),handempty],DLS),
	ord_subtract(S1,DLS,S),
	list_to_ord_set([holding(X),clear(Y)],ALS),
	ord_union(S,ALS,S2).
*/
