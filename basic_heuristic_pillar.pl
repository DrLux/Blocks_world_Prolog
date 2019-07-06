
% naive heuristic: number of blocks not currently in goal position

heuristic(S,G,Cost):-
    findall(X,block(X),BlockList),
 	compute_cost(BlockList,S,G,Cost),
 	write("heuristic: "),
 	write(Cost),
 	write("\n").

% goal_position(X) = true if the block sequence from X to pillar is the same in S and G
goal_position(X,S,G):-
	member(onpillar(X,P),G),
	ord_memberchk(onpillar(X,P),S).

goal_position(X,S,G):-
	member(on(X,Y),G),
	ord_memberchk(on(X,Y),S),
	goal_position(Y,S,G).

compute_cost([],_,_,0).

compute_cost([X|Tail],S,G,Cost):-
	goal_position(X,S,G),!,
	compute_cost(Tail,S,G,Cost).

compute_cost([_|Tail],S,G,Cost):-
	compute_cost(Tail,S,G,TailCost),
	Cost is TailCost + 1. 

