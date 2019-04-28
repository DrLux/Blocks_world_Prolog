
% naive heuristic: number of blocks not currently in goal position

heuristic(S,G,Cost):-
    findall(X,block(X),BlockList),
 	computeCost(BlockList,S,G,Cost),
 	write("heuristic: "),
 	write(Cost),
 	write("\n").

% goalPosition(X) = true if the block sequence from X to Table is the same in S and G
goalPosition(X,S,G):-
	ord_memberchk(ontable(X),G),
	ord_memberchk(ontable(X),S).

goalPosition(X,S,G):-
	%block(Y),
	member(on(X,Y),G),
	ord_memberchk(on(X,Y),S),
	goalPosition(Y,S,G).

computeCost([],_,_,0).

computeCost([X|Tail],S,G,Cost):-
	goalPosition(X,S,G),!,
	computeCost(Tail,S,G,Cost).

computeCost([_|Tail],S,G,Cost):-
	computeCost(Tail,S,G,TailCost),
	Cost is TailCost + 1. 

