% Alternate version of idastar that makes use of asserta to 
% keep track of the minimum f-value found during a limited depth first search

:- dynamic fmin/1.
:- dynamic exp_nodes/1.

init_exp_nodes :-
    retractall(exp_nodes(_)),
    asserta(exp_nodes(0)).

inc_exp_nodes :- 
    exp_nodes(E),
    retractall(exp_nodes(_)),
    NewE is E + 1,
    asserta(exp_nodes(NewE)).
    
    
idastar(Solution):-
    init_exp_nodes,
    statistics(walltime, []),
    initial(S),
    write("\nstato initial:\n"),write(S),nl,
    write("\nstato final:\n"),
    goal(G),
    write(G),nl,
    write("\nStatistics:\n"),
    heuristic(S,G,HRoot),
	idastar_aux(HRoot,Sol),
	reverse(Sol,Solution),
    length(Solution, Cost),
    write("\nCost Solution: "),
    write(Cost),
    statistics(walltime, [ _ | [ExecutionTime]]), %_ stand for NewTimeSinceStart
    write('\nExecution took '), write(ExecutionTime), write(' ms.'),
    exp_nodes(Expanded_nodes),
    write('\nExpanded nodes: '), write(Expanded_nodes),
    write('\n\nSolution: '), write(Solution).
	
idastar_aux(FLimit,RealSolution):-
    retractall(fmin(_)),
    asserta(fmin(999999)),
    ldfs(FLimit,Solution),
    idastar_choice(Solution,RealSolution).

% check if a solution was found (FMin == -1)
idastar_choice(Solution,Solution):-
    fmin(FMin),
    FMin < 0.

% if not, retry with a greater max f-value (FMin)
idastar_choice(_,Solution):-
    fmin(FMin),
    FMin < 999999,
    write("retry:  "),
    write(FMin),nl,
    idastar_aux(FMin,Solution).
    
% f-limited depth first search 
% FMin is the minimal f-value that exceeded the limit in the previous search
ldfs(FLimit,Solution):-
    initial(S),
    ldfs_aux(node(S,[],0,0),[S],FLimit,Solution).
    
% when we find a solution, we return a min f-value of -1 so 
% we can detect it and stop the search
ldfs_aux(node(S,ActionsToS,_,_),_,_,ActionsToS):-
    final(S),
    updateFMin(-1).

ldfs_aux(node(S,ActionsToS,G,H),Visited,FLimit,Solution):-
    findall(Action,applicable(Action,S),ApplicableActions),
    generateChildren(node(S,ActionsToS,G,H),Visited,ApplicableActions,FLimit,ChildrenList),
    exploreChildren(ChildrenList,Visited,FLimit,Solution).
    

generateChildren(node(_,_,_,_),_,[],_,[]).

% if we already visited the child, completely ignore it
generateChildren(node(S,ActionsToS,G,H),Visited,[Action|OtherActions],FLimit,ChildrenList):-
    transform(Action,S,NewS),
    member(NewS,Visited),!,
    generateChildren(node(S,ActionsToS,G,H),Visited,OtherActions,FLimit,ChildrenList).

generateChildren(node(S,ActionsToS,G,H),Visited,[Action|OtherActions],FLimit,ChildrenList):-
    transform(Action,S,NewS),
    getActionCost(Action,ActionCost),
    goal(Goal),
    heuristic(NewS,Goal,NewH),
    NewG is G + ActionCost,
    inc_exp_nodes, %%%%%
    generateChildren(node(S,ActionsToS,G,H),Visited,OtherActions,FLimit,ChildrenListTail),
    chooseToInsert(node(NewS,[Action|ActionsToS],NewG,NewH),FLimit,ChildrenListTail,ChildrenList).

% if the child does not exceed the f-limit, FMin must not be updated, and the child is added to the children list
chooseToInsert(node(S,A,G,H),FLimit,ChildrenListIn,ChildrenListOut):-
    G + H =< FLimit,!,
    orderedInsertNode(ChildrenListIn,node(S,A,G,H),ChildrenListOut).
    
% otherwise, FMin must be set to the minimum of the f-values, and the child is NOT added to the children list
chooseToInsert(node(_,_,G,H),_,ChildrenListIn,ChildrenListOut):-
    %G + H > FLimit,
    NewF is G + H, 
    updateFMin(NewF),
    ChildrenListOut = ChildrenListIn.


exploreChildren([],_,_,[]).

exploreChildren([node(S,A,G,H)|OtherChildren],Visited,FLimit,Solution):-
    ldfs_aux(node(S,A,G,H),[S|Visited],FLimit,SolutionChild), %repeat f-limited depth search, starting from a child
    exploreMoreCheck(OtherChildren,Visited,FLimit,SolutionChild,Solution).

% have we found a solution by doing a depth search starting from a child? If yes, stop
exploreMoreCheck(_,_,_,SolutionIn,SolutionOut):-
    fmin(FMin),
    FMin < 0,!,
    SolutionOut = SolutionIn.

% otherwise, explore the remaining children
exploreMoreCheck(OtherChildren,Visited,FLimit,_,Solution):-
    exploreChildren(OtherChildren,Visited,FLimit,Solution).


updateFMin(NewFMin):-
    fmin(OldFMin),
    NewFMin < OldFMin,!,
    retractall(fmin(_)),
    asserta(fmin(NewFMin)).

updateFMin(_).

orderedInsertNode([],Node,[Node]).

orderedInsertNode([node(S,A,G,H)|Tail],node(SP,AP,GP,HP),[node(S,A,G,H)|NewTail]):-
    F is G + H,
    FP is GP + HP,
    F < FP,!,
    orderedInsertNode(Tail,node(SP,AP,GP,HP),NewTail).

orderedInsertNode(List,Node,[Node|List]).

getActionCost(Action,Cost):-actionCost(Action,Cost).
getActionCost(_,1).

