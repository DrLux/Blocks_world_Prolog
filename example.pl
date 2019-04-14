
%%%%%% ACTIONS' COSTS %%%%%%%%

actionCost(pickup(_),1).	
actionCost(putdown(_),1).
actionCost(stack(_,_),1).
actionCost(unstack(_,_),1).


%%%%%%%% TEST 1 %%%%%%%%


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




%%%%%% TEST 2 %%%%%%%


/*
% esempio Prof. Torasso
%block(x).
%block(y).
%block(z).
block(a).
block(b).
block(c).
block(d).
%block(ezio).

initial(S):-
	list_to_ord_set([clear(b), on(b,d), on(d,c), ontable(c), handempty],S).

goal(G):- 
	list_to_ord_set([clear(b), on(b,c), on(c,d), ontable(d), handempty],G).

final(S):- goal(G), ord_subset(G,S).
*/




%%%%%%%% TEST 3 %%%%%%%%%% 


/*
block(a).
block(b).
%block(ezio).

initial(S):-
	list_to_ord_set([clear(a), on(a,b), ontable(b), handempty],S).

goal(G):- 
	list_to_ord_set([clear(b), on(b,a), ontable(a), handempty],G).

final(S):- goal(G), ord_subset(G,S).
*/




wrapperino(X,Y):-
	list_to_ord_set([clear(a), on(a,b), on(b,d), on(d,c), ontable(c), handempty],S),
	list_to_ord_set([clear(a), on(a,b), on(b,c), on(c,d), ontable(d), handempty],G),
	belowInStack(X,Y,S).
	
