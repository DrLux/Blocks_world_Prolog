
%%%%%% ACTIONS' COSTS %%%%%%%%

actionCost(pickup(_),1).	
actionCost(putdown(_),1).
actionCost(stack(_,_),1).
actionCost(unstack(_,_),1).



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


