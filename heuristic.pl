% General note: if a block variable is known to be already instantiated when a membership
% test occour, then is more convenient to use ord_memberchk.
% Otherwise, if we search for something that has a known pattern but at least one unknown
% variable, it is useful to use member to immediately find the correct substitution (if any).
% See example below:

below_in_stack(X,Y,S):- 
	ord_memberchk(on(Y,X),S). %example: here X and Y are known: use ord_memberchk

below_in_stack(X,Y,S):- 
	member(on(Y,Z),S),  % here we only know the pattern, but we do not know Z: use member
	below_in_stack(X,Z,S).


must_be_moved_once(X,S,G):-
	member(on(X,Y),S),
	member(on(X,Z),G),
	Y\=Z.

must_be_moved_once(X,S,G):-
	ord_memberchk(ontable(X),G),
	member(on(X,_),S).

must_be_moved_once(X,S,G):-
	ord_memberchk(ontable(X),S),   
	member(on(X,_),G).

% In theory, this case should never be reached
must_be_moved_once(X,S,G):-
	member(on(X,Y),G),
	\+ord_memberchk(on(X,Y),S),
	below_in_stack(Y,X,S),
	write("\n!!!!!! - If you read this, check must_be_moved_once\n").


must_be_moved_twice(X,S,G):-
	member(on(X,Y),S),
	ord_memberchk(on(X,Y),G),
	must_be_moved_once(Y,S,G).

must_be_moved_twice(X,S,G):-
	block(Y),
	X\=Y,
	below_in_stack(Y,X,S),
	must_be_moved_twice(Y,S,G).


% goal_position(X) = true if the block sequence from X to Table is the same in S and G
goal_position(X,S,G):-
	ord_memberchk(ontable(X),G),
	ord_memberchk(ontable(X),S).

goal_position(X,S,G):-
	member(on(X,Y),G),
	ord_memberchk(on(X,Y),S),
	goal_position(Y,S,G).


% mutual_prevention(A,B) = if A and B are swapped, both reach their goal positions (tested in a fake state)
mutual_prevention(X,Y,S,G):-
    %ord_memberchk(clear(X),S),
    %ord_memberchk(clear(Y),S),
	swap_blocks(X,Y,S,FAKE_S),
	goal_position(X,FAKE_S,G),
	goal_position(Y,FAKE_S,G).

swap_blocks(X,Y,S,FAKE_S):-
	replace_block(X,Y,S,SP),
	replace_block(Y,X,S,SPP),
	ord_union(SP, SPP, FAKE_S).

replace_block(X,Y,S,SP):-
	replace_above(X,Y,S,SA),
	replace_below(X,Y,S,SB),
	ord_union(SA,SB,FAKE_S),
	replace_ontable(X,Y,FAKE_S,SP).


replace_above(X,Y,S,SPP):- % if there is a block that is not Y above X
	member(on(Z,X),S),
	Y\=Z,!, 
	ord_del_element(S,on(Z,X),SP),
	ord_add_element(SP,on(Z,Y),SPP).

replace_above(X,Y,S,SPP):- % if block Y is above X
	ord_memberchk(on(Y,X),S),!,
	ord_del_element(S,on(Y,X),SP),
	ord_add_element(SP,on(X,Y),SPP).

replace_above(_,_,S,S). % if there is no block above X


replace_below(X,Y,S,SPP):-  % if there is a block that is not Y below X
	member(on(X,Z),S),
	Y\=Z,!, 
	ord_del_element(S,on(X,Z),SP),
	ord_add_element(SP,on(Y,Z),SPP).

replace_below(X,Y,S,SPP):- % if block Y is below X
	ord_memberchk(on(X,Y),S),!,
	ord_del_element(S,on(X,Y),SP),
	ord_add_element(SP,on(Y,X),SPP).

replace_below(_,_,S,S). % if there is no block below X


replace_ontable(X,Y,S,ST):-
	ord_memberchk(ontable(X),S),!,
	ord_del_element(S,ontable(X),SP),
	ord_add_element(SP,ontable(Y),ST).

replace_ontable(_,_,S,S).


%####################################


heuristic(S,G,Cost):-
    findall(X,block(X),BlockList),
 	compute_cost(BlockList,S,G,Cost).

compute_cost([],_,_,0).

compute_cost([X|Tail],S,G,Cost):-
	must_be_moved_once(X,S,G),!,
	compute_cost(Tail,S,G,TailCost),
	Cost is TailCost + 2. % if block movement consist of two phases, pickup & putdown (1 if only direct move actions are present)

compute_cost([X|Tail],S,G,Cost):-
	must_be_moved_twice(X,S,G),!,
	compute_cost(Tail,S,G,TailCost),
	Cost is TailCost + 4. % if block movement consist of two phases, pickup & putdown (2 if only direct move actions are present)

compute_cost([_|Tail],S,G,Cost):-
	compute_cost(Tail,S,G,Cost).

