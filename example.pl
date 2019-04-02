/*
block(a).
block(b).
block(c).
block(d).
block(e).

iniziale(S):-
	list_to_ord_set([on(a,b),on(b,c),ontable(c),clear(a),on(d,e),
						  ontable(e),clear(d),handempty],S).

goal(G):- list_to_ord_set([on(a,b),on(b,c),on(c,d),ontable(d),
	ontable(e)],G).

finale(S):- goal(G), ord_subset(G,S).



*/
% esempio Prof. Torasso
block(x).
block(y).
block(z).
block(a).
block(b).
block(c).
block(d).
block(ezio).


iniziale(S):-
	list_to_ord_set([clear(a), on(a,b), on(b,c), ontable(c), handempty],S).

goal(G):- 
	list_to_ord_set([clear(a), on(a,b), on(b,c), ontable(c), handempty],G).

finale(S):- goal(G), ord_subset(G,S).

wrapperino(X,Y):-
	list_to_ord_set([clear(b), on(b,d), on(d,c), on(c,a), ontable(a),handempty],S),
	list_to_ord_set([clear(a), on(a,d), on(d,c), on(c,b), ontable(b),handempty],S),
	mutualPrevention(X,Y,S,G).

/*
%primo 
list_to_ord_set([clear(a), on(a,b), ontable(b),handempty],S),
list_to_ord_set([clear(b), on(b,a), ontable(a),handempty],G),

%secondo
list_to_ord_set([clear(a), on(a,c), on(c,b), ontable(b),handempty],S),
list_to_ord_set([clear(b), on(b,c), on(c,a), ontable(a),handempty],G),

%terzo
list_to_ord_set([clear(a), on(a,b), on(b,c), ontable(c),handempty],S),
list_to_ord_set([clear(b), on(b,a), on(a,c), ontable(c),handempty],G),

%quarto
list_to_ord_set([clear(b), on(b,d), on(d,c), on(c,a), ontable(a),handempty],S),
list_to_ord_set([clear(a), on(a,d), on(d,c), on(c,b), ontable(b),handempty],S),


*/