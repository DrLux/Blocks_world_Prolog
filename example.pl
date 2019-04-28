
%%%%%% ACTIONS COSTS %%%%%%%%

actionCost(pickup(_),1).	
actionCost(putdown(_),1).
actionCost(stack(_,_),1).
actionCost(unstack(_,_),1).

/*
%%%%%%%% Esempio 1 %%%%%%%% SOLUZIONE = 8 PASSI
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
%%%%%% Esempio 2 %%%%%%% SOLUZIONE: 12 passi
block(a).
block(b).
block(c).
block(d).
block(e).


initial(S):-
	list_to_ord_set([on(a,b),on(b,c),ontable(c),clear(a),on(d,e),
						  ontable(e),clear(d),handempty],S).

goal(G):- list_to_ord_set([on(a,b),on(b,c),on(c,d),ontable(d),
	ontable(e),clear(e)],G).

final(S):- goal(G), ord_subset(G,S).

wrapperino(X,Y):-
	initial(S),
	applicable(putOnBlock(X,Y),S).
*/

/*
%%%%%%%% Esempio 3 %%%%%%%%%%  SOLUZIONE = 14 PASSI

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

/*
%%%%%%%% Esempio 4 %%%%%%%%%%  SOLUZIONE = 18 
block(a).
block(b).
block(c).
block(d).
block(e).
block(f).
block(g).
block(h).
block(i).

initial(S):-
	list_to_ord_set([clear(f), ontable(f), clear(a), on(a,b), on(b,c), ontable(c), clear(d),on(d,e), ontable(e), clear(g), on(g,h), on(h,i), ontable(i), handempty],S).

goal(G):- 
	list_to_ord_set([clear(a), on(a,b), on(b,c), on(c,g), ontable(g), clear(d), on(d,e), on(e,f), ontable(f), clear(h), on(h,i), ontable(i), handempty],G).

final(S):- goal(G), ord_subset(G,S).
*/

/*
%con astar: set_prolog_flag(stack_limit, 2_147_483_648).
%%%%%%%% Esempio 5 %%%%%%%%%%  SOLUZIONE = 22
block(a).
block(b).
block(c).

block(d).
block(e).
block(z).
block(x).

block(y).
block(k).
block(w).

initial(S):- list_to_ord_set([
	clear(a), on(a,b), on(b,c), ontable(c),
	clear(d), on(d,e), on(e,z), on(z,x), ontable(x),
	clear(y), on(y,k), on(k,w), ontable(w), handempty],S).

goal(G):- list_to_ord_set([
	clear(z), on(z,a), on(a,w), ontable(w),
	clear(d), on(d,e), on(e,y), on(y,b), ontable(b),
	clear(c), on(c,x), on(x,k), ontable(k), handempty],G).

final(S):- goal(G), ord_subset(G,S).
*/

/*
%%%%%%%% Esempio 6 %%%%%%%%%%  SOLUZIONE = 22
block(a).
block(b).
block(c).
block(d).
block(e).

block(z).
block(x).
block(h).
block(i).


block(f).
block(g).
block(y).
block(k).
block(w).

initial(S):- list_to_ord_set([
	clear(a), on(a,b), on(b,c), on(c,e), on(e,d), ontable(d),
	clear(z), on(z,x), on(x,h), on(h,i), ontable(i),
	clear(f), on(f,g), on(g,y), on(y,k), on(k,w), ontable(w), handempty],S).

goal(G):- list_to_ord_set([
	clear(a), on(a,b), on(b,c), on(c,e), on(e,d), ontable(d),
	clear(x), on(x,f), on(f,g), on(g,h), ontable(h),
	clear(z), on(z,w), on(w,y), on(y,k), on(k,i), ontable(i), handempty],G).

final(S):- goal(G), ord_subset(G,S).
*/

/*
%%%%%%%% Esempio 7 %%%%%%%%%%  SOLUZIONE = 54 
block(a).
block(b).
block(c).
block(d).
block(e).
block(f).
block(g).
block(h).
block(i).
block(l).
block(m).

block(z).
block(x).
block(y).
block(k).
block(w).

initial(S):-
	list_to_ord_set([clear(l), on(l,h), on(h,i), on(i,f), on(f,g), on(g,e), on(e,d),on(d,c),on(c,b),on(b,a),ontable(a),

	clear(k), on(k,w), on(w,y), on(y,m), on(m,x), on(x,z), ontable(z), handempty],S).

goal(G):- 
	list_to_ord_set([clear(w), on(w,k), on(k,x), on(x,z), on(z,i), on(i,d), on(d,c), on(c,g), on(g,a),ontable(a),
	
	clear(y), on(y,e), on(e,l), on(l,m), on(m,h), on(h,b), on(b,f),ontable(f), handempty],G).

final(S):- goal(G), ord_subset(G,S).
*/


%%%%%%%% Esempio 8 %%%%%%%%%%  SOLUZIONE = 
block(a).
block(b).
block(c).
block(d).
block(e).
block(f).
block(g).
block(h).
block(i).
block(l).
block(m).

block(z).
block(q).
block(p).
block(o).
block(n).

block(r).
block(s).
block(t).
block(u).
block(v).


initial(S):-
	list_to_ord_set([
	clear(l), on(l,h), on(h,i), on(i,f), on(f,g), on(g,e), on(e,d),on(d,c),on(c,b),on(b,a),
	on(a,r), on(r,s), on(s,t), on(t,u), on(u,v), ontable(v),
	clear(k), on(k,w), on(w,y), on(y,m), on(m,x), on(x,z), ontable(z), handempty],S).

goal(G):- 
	list_to_ord_set([
	clear(w), on(w,k), on(k,x), on(x,z), on(z,i), on(i,d), on(d,c), on(c,g), on(g,a),
	on(g,r), on(r,e), on(e,h), on(h,u), on(u,f), ontable(f),
	clear(y), on(y,s), on(s,l), on(l,m), on(m,t), on(t,b), on(b,v),ontable(v), handempty],G).

final(S):- goal(G), ord_subset(G,S).
