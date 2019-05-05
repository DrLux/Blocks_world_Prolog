:- dynamic exp_nodes/1.

init_exp_nodes :-
    retractall(exp_nodes(_)),
    asserta(exp_nodes(0)).

inc_exp_nodes :- 
    exp_nodes(E),
    retractall(exp_nodes(_)),
    NewE is E + 1,
    asserta(exp_nodes(NewE)).


astar(Solution):-
    init_exp_nodes,
    statistics(walltime, []),
    initial(S),
    write("\nstato initial:\n"),write(S),nl,
    write("\nstato final:\n"),
    goal(G),
    write(G),nl,
    write("\nStatistics:\n"),
    astar_aux([node(S,[],0,0)],[],Sol),
    reverse(Sol,Solution),
    statistics(walltime, [ _ | [ExecutionTime]]), %_ stand for NewTimeSinceStart
    write('\nExecution took '), write(ExecutionTime), write(' ms.'),
    exp_nodes(Expanded_nodes),
    write('\nExpanded nodes: '), write(Expanded_nodes),
    write('\n\nSolution: '), write(Solution).

% per relazione:
% statistics(walltime, Result) sets Result as a list, with the head being the total time since the Prolog instance was started, and the tail being a single-element list representing the time since the last statistics(walltime, _) call was made.


% astar_aux(Coda,ClosedList,Soluzione)
% Coda = [node(S,Azioni,G,H)|...]

astar_aux([node(S,ActionsToS,G,_)| Frontier],_,ActionsToS):-
    final(S),!,
    write("\nCost Solution: "),
    write(G),
    write("\nNodes in frontier: "),
    length(Frontier,F), 
    write(F).
    

astar_aux([node(S,ActionsToS,G,H)|OpenTail],ClosedList,Solution):-
    findall(Action,applicable(Action,S),ApplicableActionsList),
    generateChildren(node(S,ActionsToS,G,H),ApplicableActionsList,OpenTail,[node(S,ActionsToS,G,H)|ClosedList],UpdatedOpenList,UpdatedClosedList),
    astar_aux(UpdatedOpenList,UpdatedClosedList,Solution).


generateChildren(_,[],OpenList,ClosedList,OpenList,ClosedList).

% When node is already in OpenList 
generateChildren(node(S,ActionsToS,G,H),[ApplicableAction|OtherActions],OpenList,ClosedList,UpdatedOpenList,UpdatedClosedList):-
    transform(ApplicableAction,S,NewS),
    member(node(NewS,OldActions,OldG,OldH),OpenList),!,
    getActionCost(ApplicableAction,ActionCost),
    NewG is G + ActionCost,
    updateOpenList(node(NewS,OldActions,OldG,OldH),node(NewS,[ApplicableAction|ActionsToS],NewG,OldH),OpenList,NewOpenList),
    generateChildren(node(S,ActionsToS,G,H),OtherActions,NewOpenList,ClosedList,UpdatedOpenList,UpdatedClosedList).

% When node is already in ClosedList
generateChildren(node(S,ActionsToS,G,H),[ApplicableAction|OtherActions],OpenList,ClosedList,UpdatedOpenList,UpdatedClosedList):-
    transform(ApplicableAction,S,NewS),
    member(node(NewS,OldActions,OldG,OldH),ClosedList),!,
    getActionCost(ApplicableAction,ActionCost),
    NewG is G + ActionCost,
    updateClosedList(node(NewS,OldActions,OldG,OldH),node(NewS,[ApplicableAction|ActionsToS],NewG,OldH),OpenList,ClosedList,NewOpenList,NewClosedList),
    generateChildren(node(S,ActionsToS,G,H),OtherActions,NewOpenList,NewClosedList,UpdatedOpenList,UpdatedClosedList).

% When node is not in OpenList nor in ClosedList
generateChildren(node(S,ActionsToS,G,H),[ApplicableAction|OtherActions],OpenList,ClosedList,UpdatedOpenList,UpdatedClosedList):-
    transform(ApplicableAction,S,NewS),
    getActionCost(ApplicableAction,ActionCost),
    NewG is G + ActionCost,
    goal(Goal),
    heuristic(NewS,Goal,NewH),
    orderedInsertNode(OpenList,node(NewS,[ApplicableAction|ActionsToS],NewG,NewH),NewOpenList),
    inc_exp_nodes, %%%%%
    generateChildren(node(S,ActionsToS,G,H),OtherActions,NewOpenList,ClosedList,UpdatedOpenList,UpdatedClosedList).
    

updateOpenList(node(NewS,OldActions,OldG,OldH),node(NewS,NewActions,NewG,_),OpenList,NewOpenList):-
    NewG < OldG,!,
    delete(OpenList,node(NewS,OldActions,OldG,OldH),PQR),
    orderedInsertNode(PQR,node(NewS,NewActions,NewG,OldH),NewOpenList).

updateOpenList(_,_,OpenList,OpenList).


updateClosedList(node(NewS,OldActions,OldG,OldH),node(NewS,NewActions,NewG,OldH),OpenList,ClosedList,NewOpenList,NewClosedList):-
    NewG < OldG,!,
    delete(ClosedList,node(NewS,OldActions,OldG,OldH),NewClosedList),
    orderedInsertNode(OpenList,node(NewS,NewActions,NewG,OldH),NewOpenList).

updateClosedList(_,_,OpenList,ClosedList,OpenList,ClosedList).


orderedInsertNode([],Node,[Node]).

orderedInsertNode([node(S,A,G,H)|Tail],node(SP,AP,GP,HP),[node(S,A,G,H)|NewTail]):-
    F is G + H,
    FP is GP + HP,
    F < FP,!,
    orderedInsertNode(Tail,node(SP,AP,GP,HP),NewTail).

orderedInsertNode(List,Node,[Node|List]).


getActionCost(Action,Cost):-actionCost(Action,Cost).
getActionCost(_,1).

