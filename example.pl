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
%block(x).
%block(y).
%block(z).
block(a).
block(b).
block(c).
%block(d).
%block(ezio).


iniziale(S):-
	list_to_ord_set([clear(a), on(a,b), on(b,c), ontable(c), handempty],S).

goal(G):- 
	list_to_ord_set([clear(a), on(a,b), on(b,c), ontable(c), handempty],G).

finale(S):- goal(G), ord_subset(G,S).

wrapperino(X,Y):-
	list_to_ord_set([clear(a), on(a,c), ontable(c), clear(b), on(b,d), ontable(d), handempty],S),
	list_to_ord_set([clear(b), on(b,c), ontable(c), clear(a), on(a,d), ontable(d), handempty],G),
	mutualPrevention(X,Y,S,G).
	%replaceOn(X,Y,S,SPP).
	