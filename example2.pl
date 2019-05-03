
%%%%%% ACTIONS' COSTS %%%%%%%%

actionCost(pickup(_),1).	
actionCost(putdown(_),1).
actionCost(stack(_,_),1).
actionCost(unstack(_,_),1).



%%%%%%%% Esempio 1 %%%%%%%% SOLUZIONE = 4 PASSI
pillar(p).
pillar(q).
pillar(r).

block(a).
block(b).
block(c).

initial(S):-
	list_to_ord_set([on(a,b),on(b,c),onpillar(c,p),clear(a),free(q),free(r)],S).

goal(G):- 
	list_to_ord_set([on(b,a),on(a,c),onpillar(c,p),clear(b),free(q),free(r)],G).

final(S):- goal(G), ord_subset(G,S).


/*
%%%%%% Esempio 2 %%%%%%% SOLUZIONE: 7 passi
block(a).
block(b).
block(c).
block(d).
block(e).

pillar(p).
pillar(q).
pillar(r).


initial(S):-
	list_to_ord_set([on(a,b),on(b,c),onpillar(c,p),clear(a),on(d,e),
						  onpillar(e,q),clear(d),free(r)],S).

goal(G):- list_to_ord_set([on(a,b),on(b,c),on(c,d),onpillar(d,p),
	onpillar(e,q),clear(e),clear(a)],G).

final(S):- goal(G), ord_subset(G,S).
*/

/*
%%%%%%%% Esempio 3 %%%%%%%%%%  SOLUZIONE = 8 PASSI
pillar(p).
pillar(q).
pillar(r).

block(a).
block(b).
block(c).
block(d).
block(e).
block(f).

initial(S):-
	list_to_ord_set([clear(a), on(a,b), on(b,c), on(c,d), onpillar(d,p), clear(e), on(e,f), onpillar(f,q), free(r)],S).

goal(G):- 
	list_to_ord_set([clear(a), on(a,e), on(e,f), on(f,d), onpillar(d,p), clear(c), on(c,b), onpillar(b,q), free(r)],G).

final(S):- goal(G), ord_subset(G,S).
*/

/*
%%%%%%%% Esempio 4 %%%%%%%%%%  SOLUZIONE =  
block(a).
block(b).
block(c).
block(d).
block(e).
block(f).
block(g).
block(h).
block(i).

pillar(p).
pillar(q).
pillar(r).
pillar(j).
pillar(o).

initial(S):-
	list_to_ord_set([clear(f), onpillar(f,p), clear(a), on(a,b), on(b,c), onpillar(c,q), clear(d),on(d,e), onpillar(e,r), clear(g), on(g,h), on(h,i), onpillar(i,j), free(o)],S).

goal(G):- 
	list_to_ord_set([clear(a), on(a,b), on(b,c), on(c,g), onpillar(g,p), clear(d), on(d,e), on(e,f), onpillar(f,q), clear(h), on(h,i), onpillar(i,r), free(j),free(o)],G).

final(S):- goal(G), ord_subset(G,S).
*/

