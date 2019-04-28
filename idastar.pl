
idastar(Solution):-
    initial(S),
    goal(G),
    heuristic(S,G,HRoot),
	idastar_aux(HRoot,Sol),
	reverse(Sol,Solution),
    length(Solution, Cost),
    write("\nCost Solution: "),
    write(Cost),
    write("\n").
	
idastar_aux(FLimit,RealSolution):-
    ldfs(FLimit,FMin,Solution),
    idastar_choice(FMin,Solution,RealSolution).

% check if a solution was found (FMin == -1)
idastar_choice(FMin,Solution,Solution):-
    FMin < 0.

% if not, retry with a greater max f-value (FMin)
idastar_choice(FMin,_,Solution):-
    FMin < 999999,
    write("retry:  "),
    write(FMin),
    write("\n"),
    idastar_aux(FMin,Solution).
    
% f-limited depth first search 
% FMin is the minimal f-value that exceeded the limit in the previous search
ldfs(FLimit,FMin,Solution):-
    initial(S),
    ldfs_aux(node(S,[],0,0),[S],FLimit,FMin,Solution).
    
% when we find a solution, we return a min f-value of -1 so 
% we can detect it and stop the search
ldfs_aux(node(S,ActionsToS,_,_),_,_,-1,ActionsToS):-final(S).

ldfs_aux(node(S,ActionsToS,G,H),Visited,FLimit,FMin,Solution):-
    findall(Action,applicable(Action,S),ApplicableActions),
    generateChildren(node(S,ActionsToS,G,H),Visited,ApplicableActions,FLimit,FMin1,ChildrenList),
    exploreChildren(ChildrenList,Visited,FLimit,FMin2,Solution),
    FMin is min(FMin1,FMin2). 
    

generateChildren(node(_,_,_,_),_,[],_,FMin,[]):-FMin is 999999.

% if we already visited the child, completely ignore it
generateChildren(node(S,ActionsToS,G,H),Visited,[Action|OtherActions],FLimit,FMin,ChildrenList):-
    transform(Action,S,NewS),
    member(NewS,Visited),!,
    generateChildren(node(S,ActionsToS,G,H),Visited,OtherActions,FLimit,FMin,ChildrenList).

generateChildren(node(S,ActionsToS,G,H),Visited,[Action|OtherActions],FLimit,FMin,ChildrenList):-
    transform(Action,S,NewS),
    getActionCost(Action,ActionCost),
    goal(Goal),
    heuristic(NewS,Goal,NewH),
    NewG is G + ActionCost,
    generateChildren(node(S,ActionsToS,G,H),Visited,OtherActions,FLimit,FMinTail,ChildrenListTail),
    chooseToInsert(node(NewS,[Action|ActionsToS],NewG,NewH),FLimit,FMinTail,ChildrenListTail,FMin,ChildrenList).

% if the child does not exceed the f-limit, FMin must not be updated, and the child is added to the children list
chooseToInsert(node(S,A,G,H),FLimit,FMinIn,ChildrenListIn,FMinOut,ChildrenListOut):-
    G + H =< FLimit,!,
    FMinOut is FMinIn,
    orderedInsertNode(ChildrenListIn,node(S,A,G,H),ChildrenListOut).
    
% otherwise, FMin must be set to the minimum of the f-values, and the child is NOT added to the children list
chooseToInsert(node(_,_,G,H),_,FMinIn,ChildrenListIn,FMinOut,ChildrenListOut):-
    %G + H > FLimit,
    FMinOut is min(G+H,FMinIn),
    ChildrenListOut = ChildrenListIn.


exploreChildren([],_,_,FMin,[]):-FMin is 999999.

exploreChildren([node(S,A,G,H)|OtherChildren],Visited,FLimit,FMin,Solution):-
    ldfs_aux(node(S,A,G,H),[S|Visited],FLimit,FMin1,Solution1), %repeat f-limited depth search, starting from a child
    exploreMoreCheck(OtherChildren,Visited,FLimit,FMin1,Solution1,FMin,Solution).

% have we found a solution by doing a depth search starting from a child? If yes, stop
exploreMoreCheck(_,_,_,FMinIn,SolutionIn,FMinOut,SolutionOut):-
    FMinIn < 0,!,
    FMinOut is FMinIn,
    SolutionOut = SolutionIn.

% otherwise, explore the remaining children keeping track of the minimal f-value
% that exceeds the limit that we have found along the way
exploreMoreCheck(OtherChildren,Visited,FLimit,FMinIn,_,FMin,Solution):-
    exploreChildren(OtherChildren,Visited,FLimit,FMinSibling,Solution),
    FMin is min(FMinIn,FMinSibling).


orderedInsertNode([],Node,[Node]).

orderedInsertNode([node(S,A,G,H)|Tail],node(SP,AP,GP,HP),[node(S,A,G,H)|NewTail]):-
    F is G + H,
    FP is GP + HP,
    F < FP,!,
    orderedInsertNode(Tail,node(SP,AP,GP,HP),NewTail).

orderedInsertNode(List,Node,[Node|List]).

getActionCost(Action,Cost):-actionCost(Action,Cost).
getActionCost(_,1).

