% General note: if a block variable is known to be already instantiated when a membership
% test occour, then is more convenient to use ord_memberchk.
% Otherwise, if we search for something that has a known pattern but at least one unknown
% variable, it is useful to use member to immediately find the correct substitution (if any).
% Aee example below:

belowInStack(X,Y,S):- 
	ord_memberchk(on(Y,X),S). %example: here X and Y are known: use ord_memberchk

belowInStack(X,Y,S):- 
	member(on(Y,Z),S),  % here we only know the pattern, but we do not know Z: use member
	belowInStack(X,Z,S).


mustBeMovedOnce(X,S,G):-
	member(on(X,Y),S),
	member(on(X,Z),G),
	Y\=Z.

mustBeMovedOnce(X,S,G):-
	member(onpillar(X,P),S),
	member(onpillar(X,Q),G),
	P\=Q.

% In theory, this case should never be reached
mustBeMovedOnce(X,S,G):-
	member(on(X,Y),G),
	\+ord_memberchk(on(X,Y),S),
	belowInStack(Y,X,S),
	write("\n!!!!!! - If you read this, check mustBeMovedOnce\n").


mustBeMovedTwice(X,S,G):-
	member(on(X,Y),S),
	ord_memberchk(on(X,Y),G),
	mustBeMovedOnce(Y,S,G).

mustBeMovedTwice(X,S,G):-
	block(Y),
	X\=Y,
	belowInStack(Y,X,S),
	mustBeMovedTwice(Y,S,G).


% goalPosition(A) = true if the block sequence from A to pillar is the same in S and G
goalPosition(X,S,G):-
	member(onpillar(X,P),G),
	member(onpillar(X,P),S).

goalPosition(X,S,G):-
	member(on(X,Y),G),
	ord_memberchk(on(X,Y),S),
	goalPosition(Y,S,G).


% mutualPrevention(A,B) = if A and B are swapped, both reach their goal positions (tested in a fake state)
mutualPrevention(X,Y,S,G):-
    %ord_memberchk(clear(X),S),
    %ord_memberchk(clear(Y),S),
	swapBlocks(X,Y,S,FAKE_S),
	goalPosition(X,FAKE_S,G),
	goalPosition(Y,FAKE_S,G).

swapBlocks(X,Y,S,FAKE_S):-
	replaceBlock(X,Y,S,SP),
	replaceBlock(Y,X,S,SPP),
	ord_union(SP, SPP, FAKE_S).

replaceBlock(X,Y,S,SP):-
	replaceAbove(X,Y,S,SA),
	replaceBelow(X,Y,S,SB),
	ord_union(SA,SB,FAKE_S),
	replaceOnPillar(X,Y,FAKE_S,SP).


replaceAbove(X,Y,S,SPP):- % if there is a block that is not Y above X
	member(on(Z,X),S),
	Y\=Z,!, 
	ord_del_element(S,on(Z,X),SP),
	ord_add_element(SP,on(Z,Y),SPP).

replaceAbove(X,Y,S,SPP):- % if block Y is above X
	ord_memberchk(on(Y,X),S),!,
	ord_del_element(S,on(Y,X),SP),
	ord_add_element(SP,on(X,Y),SPP).

replaceAbove(_,_,S,S). % if there is no block above X


replaceBelow(X,Y,S,SPP):-  % if there is a block that is not Y below X
	member(on(X,Z),S),
	Y\=Z,!, 
	ord_del_element(S,on(X,Z),SP),
	ord_add_element(SP,on(Y,Z),SPP).

replaceBelow(X,Y,S,SPP):- % if block Y is below X
	ord_memberchk(on(X,Y),S),!,
	ord_del_element(S,on(X,Y),SP),
	ord_add_element(SP,on(Y,X),SPP).

replaceBelow(_,_,S,S). % if there is no block below X


replaceOnPillar(X,Y,S,ST):-
	member(onpillar(X,P),S),!,
	ord_del_element(S,onpillar(X,P),SP),
	ord_add_element(SP,onpillar(Y,P),ST).

replaceOnPillar(_,_,S,S).


%####################################


heuristic(S,G,Cost):-
    findall(X,block(X),BlockList),
 	computeCost(BlockList,S,G,Cost).

computeCost([],_,_,0).

computeCost([X|Tail],S,G,Cost):-
	mustBeMovedTwice(X,S,G),!,
	computeCost(Tail,S,G,TailCost),
	Cost is TailCost + 2. % if block movement consist of two phases, pickup & putdown (2 if only direct move actions are present)

computeCost([X|Tail],S,G,Cost):-
	mustBeMovedOnce(X,S,G),!,
	computeCost(Tail,S,G,TailCost),
	Cost is TailCost + 1. % if block movement consist of two phases, pickup & putdown (1 if only direct move actions are present)

computeCost([_|Tail],S,G,Cost):-
	computeCost(Tail,S,G,Cost).

