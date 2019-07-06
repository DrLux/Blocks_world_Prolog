% Alternate version of idastar that makes NO use of asserta to 
% keep track of the minimum f-value found during a limited depth first search

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
	statistics(walltime, [ _ | [ExecutionTime]]), % _ stands for NewTimeSinceStart
	reverse(Sol,Solution),
    write('\nExecution took '), write(ExecutionTime), write(' ms.'),
    exp_nodes(Expanded_nodes),
    write('\nExpanded nodes: '), write(Expanded_nodes),
    write('\n\nSolution: '), write(Solution).
	
idastar_aux(FLimit,RealSolution):-
    ldfs(FLimit,FMin,Solution),
    idastar_choice(FMin,Solution,RealSolution).


% check if a solution was found (FMin == -1)
idastar_choice(FMin,Solution,Solution):-
    FMin < 0,!.

% if not, retry with a greater f-value limit (FMin)
idastar_choice(FMin,_,Solution):-
    infinity(Infinity),
    FMin < Infinity,
    write("Retry with f limit: "), write(FMin), nl,
    idastar_aux(FMin,Solution).
    
    
% f-limited depth first search 
% FMin is the minimal f-value that exceeded the limit in the previous search
ldfs(FLimit,FMin,Solution):-
    initial(S),
    ldfs_aux(node(S,[],0,0),[S],FLimit,FMin,Solution).
    
    
% when we find a solution, we return a min f-value of -1 so 
% we can detect it and stop the search
ldfs_aux(node(S,ActionsToS,G,_),_,_,FMin,ActionsToS):-
    final(S),
    FMin is -1,
    write("\nSolution cost: "), write(G).

ldfs_aux(node(S,ActionsToS,G,H),Visited,FLimit,FMin,Solution):-
    findall(Action,applicable(Action,S),ApplicableActions),
    generate_children(node(S,ActionsToS,G,H),Visited,ApplicableActions,FLimit,FMin1,ChildrenList),
    explore_children(ChildrenList,Visited,FLimit,FMin2,Solution),
    FMin is min(FMin1,FMin2). 
    

generate_children(node(_,_,_,_),_,[],_,FMin,[]):-
    infinity(Infinity),
    FMin is Infinity.

% if we already visited the child, completely ignore it
generate_children(node(S,ActionsToS,G,H),Visited,[Action|OtherActions],FLimit,FMin,ChildrenList):-
    transform(Action,S,NewS),
    member(NewS,Visited),!,
    generate_children(node(S,ActionsToS,G,H),Visited,OtherActions,FLimit,FMin,ChildrenList).

generate_children(node(S,ActionsToS,G,H),Visited,[Action|OtherActions],FLimit,FMin,ChildrenList):-
    transform(Action,S,NewS),
    get_action_cost(Action,ActionCost),
    goal(Goal),
    heuristic(NewS,Goal,NewH),
    NewG is G + ActionCost,
    inc_exp_nodes, %%%%%
    generate_children(node(S,ActionsToS,G,H),Visited,OtherActions,FLimit,FMinTail,ChildrenListTail),
    choose_to_insert(node(NewS,[Action|ActionsToS],NewG,NewH),FLimit,FMinTail,ChildrenListTail,FMin,ChildrenList).


% if the child does not exceed the f-limit, FMin must not be updated, and the child is added to the children list
choose_to_insert(node(S,A,G,H),FLimit,FMinIn,ChildrenListIn,FMinOut,ChildrenListOut):-
    G + H =< FLimit,!,
    FMinOut is FMinIn,
    ordered_insert_node(ChildrenListIn,node(S,A,G,H),ChildrenListOut).
    
% otherwise, FMin must be set to the minimum of the f-values, and the child is NOT added to the children list
choose_to_insert(node(_,_,G,H),_,FMinIn,ChildrenListIn,FMinOut,ChildrenListOut):-
    %G + H > FLimit,
    FMinOut is min(G+H,FMinIn),
    ChildrenListOut = ChildrenListIn.


explore_children([],_,_,FMin,[]):-
    infinity(Infinity),
    FMin is Infinity.

explore_children([node(S,A,G,H)|OtherChildren],Visited,FLimit,FMin,Solution):-
    ldfs_aux(node(S,A,G,H),[S|Visited],FLimit,FMin1,Solution1), %repeat f-limited depth search, starting from a child
    explore_more_check(OtherChildren,Visited,FLimit,FMin1,Solution1,FMin,Solution).


% have we found a solution by doing a depth search starting from a child? If yes, stop
explore_more_check(_,_,_,FMinIn,SolutionIn,FMinOut,SolutionOut):-
    FMinIn < 0,!,
    FMinOut is FMinIn,
    SolutionOut = SolutionIn.

% otherwise, explore the remaining children keeping track of the minimal f-value
% that exceeds the limit that we have found along the way
explore_more_check(OtherChildren,Visited,FLimit,FMinIn,_,FMin,Solution):-
    explore_children(OtherChildren,Visited,FLimit,FMinSibling,Solution),
    FMin is min(FMinIn,FMinSibling).


ordered_insert_node([],Node,[Node]).

ordered_insert_node([node(S,A,G,H)|Tail],node(SP,AP,GP,HP),[node(S,A,G,H)|NewTail]):-
    F is G + H,
    FP is GP + HP,
    F < FP,!,
    ordered_insert_node(Tail,node(SP,AP,GP,HP),NewTail).

ordered_insert_node(List,Node,[Node|List]).


get_action_cost(Action,Cost):-action_cost(Action,Cost).
get_action_cost(_,1).

