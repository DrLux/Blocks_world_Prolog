
:- dynamic fmin/1.  % used to keep track of current f-min value
:- dynamic exp_nodes/1.

infinity(9999999).

init_exp_nodes:-
    retractall(exp_nodes(_)),
    asserta(exp_nodes(0)).

inc_exp_nodes:- 
    exp_nodes(E),
    retractall(exp_nodes(_)),
    NewE is E + 1,
    asserta(exp_nodes(NewE)).
    
    
idastar(Solution):-
    init_exp_nodes,
    initial(S),
    goal(G),
    write("\nInitial state:\n"), write(S), nl,
    write("\nGoal state:\n"), write(G), nl,
    write("\nStatistics:\n"),
    statistics(walltime, []),
    heuristic(S,G,HRoot),
	idastar_aux(HRoot,Sol),
	statistics(walltime, [ _ | [ExecutionTime]]), %_ stand for NewTimeSinceStart
	reverse(Sol,Solution),
    write('\nExecution took '), write(ExecutionTime), write(' ms.'),
    exp_nodes(Expanded_nodes),
    write('\nExpanded nodes: '), write(Expanded_nodes),
    write('\n\nSolution: '), write(Solution).


% search for a solution with f-value limit
idastar_aux(FLimit,Solution):-
    retractall(fmin(_)),
    infinity(Infinity),
    asserta(fmin(Infinity)),
    ldfs(FLimit,Solution),!.
    
% if not found, retry with a greater f-value limit (FMin)
idastar_aux(_,Solution):-
    fmin(FMin),
    infinity(Infinity),
    FMin < Infinity,
    write("\nRetry with f limit: "), write(FMin), nl,
    idastar_aux(FMin,Solution).
    
    
% f-limited depth first search 
% FMin is the minimal f-value that exceeded the limit in the previous search
ldfs(FLimit,Solution):-
    initial(S),
    ldfs_aux(node(S,[],0,0),[S],FLimit,Solution).
    
    
ldfs_aux(node(S,ActionsToS,G,_),_,_,ActionsToS):-
    final(S),!,
    write("\nSolution cost: "), write(G).

ldfs_aux(node(S,ActionsToS,G,H),Visited,FLimit,Solution):-
    findall(Action,applicable(Action,S),ApplicableActions),
    generate_children(node(S,ActionsToS,G,H),Visited,ApplicableActions,FLimit,ChildrenList),!,
    explore_children(ChildrenList,Visited,FLimit,Solution).
    

generate_children(node(_,_,_,_),_,[],_,[]).

% if we already visited the child, completely ignore it
generate_children(node(S,ActionsToS,G,H),Visited,[Action|OtherActions],FLimit,ChildrenList):-
    transform(Action,S,NewS),
    member(NewS,Visited),!,
    generate_children(node(S,ActionsToS,G,H),Visited,OtherActions,FLimit,ChildrenList).

% otherwise, proceed to generate the other children and then test if the child must be inserted in the list
generate_children(node(S,ActionsToS,G,H),Visited,[Action|OtherActions],FLimit,ChildrenList):-
    transform(Action,S,NewS),
    get_action_cost(Action,ActionCost),
    goal(Goal),
    heuristic(NewS,Goal,NewH),
    NewG is G + ActionCost,
    inc_exp_nodes, %%%%%
    generate_children(node(S,ActionsToS,G,H),Visited,OtherActions,FLimit,ChildrenListTail),
    choose_to_insert(node(NewS,[Action|ActionsToS],NewG,NewH),FLimit,ChildrenListTail,ChildrenList).


% if the child does not exceed the f-limit, FMin must not be updated, and the child is added to the children list
choose_to_insert(node(S,A,G,H),FLimit,ChildrenListIn,ChildrenListOut):-
    G + H =< FLimit,!,
    ordered_insert_node(ChildrenListIn,node(S,A,G,H),ChildrenListOut).
    
% otherwise, FMin must be set to the minimum of the f-values, and the child is NOT added to the children list
choose_to_insert(node(_,_,G,H),_,ChildrenListIn,ChildrenListOut):-
    % G + H > FLimit,
    NewF is G + H, 
    update_fmin(NewF),
    ChildrenListOut = ChildrenListIn.


explore_children([],_,_,[]):- fail.

% children are explored in order, starting from the most promising one
explore_children([node(S,A,G,H)|_],Visited,FLimit,Solution):-
    ldfs_aux(node(S,A,G,H),[S|Visited],FLimit,Solution),!. %repeat f-limited depth search, starting from a child. If success, cut.

% if no success, try another child
explore_children([_|OtherChildren],Visited,FLimit,Solution):-
    explore_children(OtherChildren,Visited,FLimit,Solution).


update_fmin(NewFMin):-
    fmin(OldFMin),
    NewFMin < OldFMin,!,
    retractall(fmin(_)),
    asserta(fmin(NewFMin)).

update_fmin(_).


ordered_insert_node([],Node,[Node]).

ordered_insert_node([node(S,A,G,H)|Tail],node(SP,AP,GP,HP),[node(S,A,G,H)|NewTail]):-
    F is G + H,
    FP is GP + HP,
    F < FP,!,
    ordered_insert_node(Tail,node(SP,AP,GP,HP),NewTail).

ordered_insert_node(List,Node,[Node|List]).


get_action_cost(Action,Cost):-action_cost(Action,Cost).
get_action_cost(_,1).

