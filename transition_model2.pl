
applicable(putOnBlock(X,Y),S):-
	block(X),
	block(Y),
	X\=Y,
	ord_memberchk(clear(X),S), 
	ord_memberchk(clear(Y),S).

applicable(putOnPillar(X,P),S):-
	block(X),
	pillar(P),
	ord_memberchk(clear(X),S),
	ord_memberchk(free(P),S).
	
	
% if X was above another block
transform(putOnBlock(X,Y),S1,S2):-
	member(on(X,Z),S1),
	X\=Z,!,
	list_to_ord_set([on(X,Z),clear(Y)],DLS),
	ord_subtract(S1,DLS,S),
	list_to_ord_set([on(X,Y),clear(Z)],ALS),
	ord_union(S,ALS,S2).


% if X was above a pillar
transform(putOnBlock(X,Y),S1,S2):-
	member(onpillar(X,P),S1), 
	list_to_ord_set([onpillar(X,P),clear(Y)],DLS),
	ord_subtract(S1,DLS,S),
	list_to_ord_set([on(X,Y),free(P)],ALS),
	ord_union(S,ALS,S2).


% if X was above another block
transform(putOnPillar(X,P),S1,S2):-
	member(on(X,Z),S1),
	X\=Z,!,
	list_to_ord_set([on(X,Z),free(P)],DLS),
	ord_subtract(S1,DLS,S),
	list_to_ord_set([onpillar(X,P),clear(Z)],ALS),
	ord_union(S,ALS,S2).

% if X was above a pillar
transform(putOnPillar(X,P),S1,S2):-
	member(onpillar(X,R),S1),
	list_to_ord_set([onpillar(X,R),free(P)],DLS),
	ord_subtract(S1,DLS,S),
	list_to_ord_set([onpillar(X,P),free(R)],ALS),
	ord_union(S,ALS,S2).
	

