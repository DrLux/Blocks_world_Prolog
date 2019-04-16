
%%%%%% ACTIONS' COSTS %%%%%%%%

actionCost(pickup(_),1).	
actionCost(putdown(_),1).
actionCost(stack(_,_),1).
actionCost(unstack(_,_),1).


/*
%%%%% TEST BASE CON I PILASTRI %%%%%%
pillar(p).
pillar(q).
pillar(r).

block(a).
block(b).
block(c).

initial(S):-
	list_to_ord_set([on(a,b),on(b,c),onpillar(c,p),clear(a),clear(q),clear(r)],S).

goal(G):- 
	list_to_ord_set([on(b,a),on(a,c),onpillar(c,p),clear(b),clear(q),clear(r)],G).

final(S):- goal(G), ord_subset(G,S).

wrapperino(ApplicableActionsList):-
	initial(S),
	findall(Action,applicable(Action,S),ApplicableActionsList).
%%%%% FINE TEST CON I PILASTRI %%%%%%
*/
/*
%%%%%% TEST 1 CON I PILASTRI %%%%%%%


block(a).
block(b).
block(c).
block(d).
block(e).

pillar(p).
pillar(q).
pillar(r).


initial(S):-
	list_to_ord_set([clear(a),on(a,b),on(b,c),onpillar(c,p),clear(d),on(d,e),onpillar(e,q),clear(r)],S).

goal(G):- 
	list_to_ord_set([clear(a),on(a,b),on(b,c),on(c,d),onpillar(d,p),clear(e),onpillar(e,q),clear(r)],G).


final(S):- goal(G), ord_subset(G,S).

wrapperino(X,Y):-
	initial(S),
	applicable(putOnBlock(X,Y),S).
*/


%%%%%%%% TEST HARD COI PILASTRI %%%%%%%%%%  SOLUZIONE = 14 PASSI

block(a).
block(b).
block(c).
block(d).
block(e).
block(f).

pillar(p).
pillar(q).
pillar(r).

initial(S):-
	list_to_ord_set([clear(a), on(a,b), on(b,c), on(c,d), onpillar(d,p), clear(e), on(e,f), onpillar(f,q), clear(r)],S).

goal(G):- 
	list_to_ord_set([clear(a), on(a,e), on(e,f), on(f,d), onpillar(d,p), clear(c), on(c,b), onpillar(b,q), clear(r)],G).

final(S):- goal(G), ord_subset(G,S).



/*
%%%%%%%% TEST BASE %%%%%%%% SOLUZIONE = 8 PASSI

block(a).
block(b).
block(c).

initial(S):-
	list_to_ord_set([on(a,b),on(b,c),ontable(c),clear(a),handempty],S).

goal(G):- 
	list_to_ord_set([on(b,a),on(a,c),ontable(c),clear(b),handempty],G).

final(S):- goal(G), ord_subset(G,S).

wrapperino(ApplicableActionsList):-
	initial(S),
	findall(Action,applicable(Action,S),ApplicableActionsList).
*/


/*
%%%%%% TEST 1 %%%%%%% SOLUZIONE: 12 passi

block(a).
block(b).
block(c).
block(d).
block(e).


initial(S):-
	list_to_ord_set([on(a,b),on(b,c),ontable(c),clear(a),on(d,e),
						  ontable(e),clear(d),handempty],S).

goal(G):- list_to_ord_set([on(a,b),on(b,c),on(c,d),ontable(d),
	ontable(e)],G).

final(S):- goal(G), ord_subset(G,S).

wrapperino(X,Y):-
	initial(S),
	applicable(putOnBlock(X,Y),S).
*/


/*
%%%%%%%% TEST HARD %%%%%%%%%%  SOLUZIONE = 14 PASSI

block(a).
block(b).
block(c).
block(d).
block(e).
block(f).

initial(S):-
	list_to_ord_set([clear(a), on(a,b), on(b,c), on(c,d), ontable(d), clear(e), on(e,f), ontable(f), handempty],S).

goal(G):- 
	list_to_ord_set([clear(a), on(a,e), on(e,f), on(f,d), ontable(d), clear(c), on(c,b), ontable(b), handempty],G).

final(S):- goal(G), ord_subset(G,S).

*/
