
idastar(MaxLimit,Step,Solution):-
	idastar_aux(1,MaxLimit,Step,Solution).

idastar_aux(Limit,_,_,Solution):-
	limited_astar(Limit,Solution),!.

idastar_aux(Limit,MaxLimit,Step,Solution):-
	Limit<MaxLimit,
	NewLimit is Limit+Step,
	idastar_aux(NewLimit,MaxLimit,Step,Solution).

	
limited_astar(Limit,Solution):-
    initial(S),
    write("\nstato initial:\n"),
    write(S),
    write("\nstato final:\n"),
    goal(G),
    write(G),
    limited_astar_aux([node(S,[],0,0)],[],Limit,Sol),
    reverse(Sol,Solution).

% limited_astar_aux(Coda,ClosedList,Soluzione)
% Coda = [node(S,Azioni,G,H)|...]

limited_astar_aux([node(S,ActionsToS,G,_)|_],_,_,ActionsToS):-
    final(S),!,
    write("\nCost Solution: "),
    write(G),
    write("\n").

limited_astar_aux([node(S,ActionsToS,G,H)|OpenTail],ClosedList,Limit,Solution):-
    findall(Action,applicable(Action,S),ApplicableActionsList),
    generateChildren(node(S,ActionsToS,G,H),ApplicableActionsList,Limit,OpenTail,[node(S,ActionsToS,G,H)|ClosedList],UpdatedOpenList,UpdatedClosedList),
    limited_astar_aux(UpdatedOpenList,UpdatedClosedList,Limit,Solution).


generateChildren(_,[],_,OpenList,ClosedList,OpenList,ClosedList).

%When node is already in OpenList 
generateChildren(node(S,ActionsToS,G,H),[ApplicableAction|OtherActions],Limit,OpenList,ClosedList,UpdatedOpenList,UpdatedClosedList):-
    transform(ApplicableAction,S,NewS),
    member(node(NewS,OldActions,OldG,OldH),OpenList),!,
    getActionCost(ApplicableAction,ActionCost),
    NewG is G + ActionCost,
    updateOpenList(node(NewS,OldActions,OldG,OldH),node(NewS,[ApplicableAction|ActionsToS],NewG,OldH),OpenList,NewOpenList),
    generateChildren(node(S,ActionsToS,G,H),OtherActions,Limit,NewOpenList,ClosedList,UpdatedOpenList,UpdatedClosedList).

%When node is already in ClosedList
generateChildren(node(S,ActionsToS,G,H),[ApplicableAction|OtherActions],Limit,OpenList,ClosedList,UpdatedOpenList,UpdatedClosedList):-
    transform(ApplicableAction,S,NewS),
    member(node(NewS,OldActions,OldG,OldH),ClosedList),!,
    getActionCost(ApplicableAction,ActionCost),
    NewG is G + ActionCost,
    updateClosedList(node(NewS,OldActions,OldG,OldH),node(NewS,[ApplicableAction|ActionsToS],NewG,OldH),OpenList,ClosedList,NewOpenList,NewClosedList),
    generateChildren(node(S,ActionsToS,G,H),OtherActions,Limit,NewOpenList,NewClosedList,UpdatedOpenList,UpdatedClosedList).

%When node is not in OpenList nor in ClosedList
generateChildren(node(S,ActionsToS,G,H),[ApplicableAction|OtherActions],Limit,OpenList,ClosedList,UpdatedOpenList,UpdatedClosedList):-
    transform(ApplicableAction,S,NewS),
    getActionCost(ApplicableAction,ActionCost),
    NewG is G + ActionCost,
    goal(Goal),
    heuristic(NewS,Goal,NewH),
    NewG + NewH =< Limit,!,
    orderedInsertNode(OpenList,node(NewS,[ApplicableAction|ActionsToS],NewG,NewH),NewOpenList),
    generateChildren(node(S,ActionsToS,G,H),OtherActions,Limit,NewOpenList,ClosedList,UpdatedOpenList,UpdatedClosedList).
    
%When one node exceded limit    
generateChildren(node(S,ActionsToS,G,H),[_|OtherActions],Limit,OpenList,ClosedList,UpdatedOpenList,UpdatedClosedList):-
    generateChildren(node(S,ActionsToS,G,H),OtherActions,Limit,OpenList,ClosedList,UpdatedOpenList,UpdatedClosedList).



getActionCost(Action,Cost):-actionCost(Action,Cost).
getActionCost(_,1).


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


orderedInsertNode([],node(S,A,G,H),[node(S,A,G,H)]).

orderedInsertNode([node(S,A,G,H)|Tail],node(SP,AP,GP,HP),[node(S,A,G,H)|NewTail]):-
    F is G + H,
    FP is GP + HP,
    F < FP,!,
    orderedInsertNode(Tail,node(SP,AP,GP,HP),NewTail).

orderedInsertNode(List,node(S,A,G,H),[node(S,A,G,H)|List]).
