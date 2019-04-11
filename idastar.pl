
idastar(Solution):-
    initial(S),
    goal(G),
    heuristic(S,G,RootH),
	idastar_aux(RootH,Solution).
	
idastar_aux(FLimit,FFound,Solution):-
    
idastar_aux(

limited_depth_first_search(Limit,Solution):-
    initial(S),
    ldfs_aux(S,[S],Limit,Solution).

ldfs_aux(S,_,_,[]):-final(S).

ldfs_aux(S,Visited,Limit,[Action|ActionsTail]):-
    Limit>0,
    applicable(Action,S),
    trasform(Action,S,NewS),
    \+member(NewS,Visited),
    DecrementedLimit is Limit-1,
    ldfs_aux(NewS,[NewS|Visited],DecrementedLimit,ActionsTail).








getActionCost(Action,Cost):-actionCost(Action,Cost).
getActionCost(_,1).


