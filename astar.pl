
:- dynamic exp_nodes/1.

init_exp_nodes:-
    retractall(exp_nodes(_)),
    asserta(exp_nodes(0)).

inc_exp_nodes:- 
    exp_nodes(E),
    retractall(exp_nodes(_)),
    NewE is E + 1,
    asserta(exp_nodes(NewE)).


% statistics(walltime, Result) sets Result as a list, with the head being the total time since 
% the Prolog instance was started, and the tail being a single-element 
% list representing the time since the last statistics(walltime, _) call was made.
astar(Solution):-
    init_exp_nodes,
    initial(S),
    goal(G),
    write("\nInitial state:\n"), write(S), nl,
    write("\nGoal state:\n"), write(G), nl,
    write("\nStatistics:\n"),
    statistics(walltime, []),
    astar_aux([node(S,[],0,0)],[],Sol),
    statistics(walltime, [ _ | [ExecutionTime]]), % _ stands for NewTimeSinceStart
    reverse(Sol,Solution),
    write('\nExecution took '), write(ExecutionTime), write(' ms.'),
    exp_nodes(Expanded_nodes),
    write('\nExpanded nodes: '), write(Expanded_nodes),
    write('\n\nSolution: '), write(Solution).


astar_aux([node(S,ActionsToS,G,_)| Frontier],_,ActionsToS):-
    final(S),!,
    write("\nSolution cost: "), write(G),
    length(Frontier,F), 
    write("\nNodes in frontier: "), write(F).
    
astar_aux([node(S,ActionsToS,G,H)|OpenTail],ClosedList,Solution):-
    findall(Action,applicable(Action,S),ApplicableActionsList),
    generate_children(node(S,ActionsToS,G,H),ApplicableActionsList,OpenTail,[node(S,ActionsToS,G,H)|ClosedList],UpdatedOpenList,UpdatedClosedList),
    astar_aux(UpdatedOpenList,UpdatedClosedList,Solution).


generate_children(_,[],OpenList,ClosedList,OpenList,ClosedList).

% When node is already in OpenList 
generate_children(node(S,ActionsToS,G,H),[ApplicableAction|OtherActions],OpenList,ClosedList,UpdatedOpenList,UpdatedClosedList):-
    transform(ApplicableAction,S,NewS),
    member(node(NewS,OldActions,OldG,OldH),OpenList),!,
    get_action_cost(ApplicableAction,ActionCost),
    NewG is G + ActionCost,
    update_open_list(node(NewS,OldActions,OldG,OldH),node(NewS,[ApplicableAction|ActionsToS],NewG,OldH),OpenList,NewOpenList),
    generate_children(node(S,ActionsToS,G,H),OtherActions,NewOpenList,ClosedList,UpdatedOpenList,UpdatedClosedList).

% When node is already in ClosedList
generate_children(node(S,ActionsToS,G,H),[ApplicableAction|OtherActions],OpenList,ClosedList,UpdatedOpenList,UpdatedClosedList):-
    transform(ApplicableAction,S,NewS),
    member(node(NewS,OldActions,OldG,OldH),ClosedList),!,
    get_action_cost(ApplicableAction,ActionCost),
    NewG is G + ActionCost,
    update_closed_list(node(NewS,OldActions,OldG,OldH),node(NewS,[ApplicableAction|ActionsToS],NewG,OldH),OpenList,ClosedList,NewOpenList,NewClosedList),
    generate_children(node(S,ActionsToS,G,H),OtherActions,NewOpenList,NewClosedList,UpdatedOpenList,UpdatedClosedList).

% When node is not in OpenList nor in ClosedList
generate_children(node(S,ActionsToS,G,H),[ApplicableAction|OtherActions],OpenList,ClosedList,UpdatedOpenList,UpdatedClosedList):-
    transform(ApplicableAction,S,NewS),
    get_action_cost(ApplicableAction,ActionCost),
    NewG is G + ActionCost,
    goal(Goal),
    heuristic(NewS,Goal,NewH),
    ordered_insert_node(OpenList,node(NewS,[ApplicableAction|ActionsToS],NewG,NewH),NewOpenList),
    inc_exp_nodes, %%%%%
    generate_children(node(S,ActionsToS,G,H),OtherActions,NewOpenList,ClosedList,UpdatedOpenList,UpdatedClosedList).
    

update_open_list(node(NewS,OldActions,OldG,OldH),node(NewS,NewActions,NewG,_),OpenList,NewOpenList):-
    NewG < OldG,!,
    delete(OpenList,node(NewS,OldActions,OldG,OldH),PQR),
    ordered_insert_node(PQR,node(NewS,NewActions,NewG,OldH),NewOpenList).

update_open_list(_,_,OpenList,OpenList).


update_closed_list(node(NewS,OldActions,OldG,OldH),node(NewS,NewActions,NewG,OldH),OpenList,ClosedList,NewOpenList,NewClosedList):-
    NewG < OldG,!,
    delete(ClosedList,node(NewS,OldActions,OldG,OldH),NewClosedList),
    ordered_insert_node(OpenList,node(NewS,NewActions,NewG,OldH),NewOpenList).

update_closed_list(_,_,OpenList,ClosedList,OpenList,ClosedList).


ordered_insert_node([],Node,[Node]).

ordered_insert_node([node(S,A,G,H)|Tail],node(SP,AP,GP,HP),[node(S,A,G,H)|NewTail]):-
    F is G + H,
    FP is GP + HP,
    F < FP,!,
    ordered_insert_node(Tail,node(SP,AP,GP,HP),NewTail).

ordered_insert_node(List,Node,[Node|List]).


get_action_cost(Action,Cost):-action_cost(Action,Cost).
get_action_cost(_,1).

