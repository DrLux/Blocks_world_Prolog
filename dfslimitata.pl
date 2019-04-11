% ldfs_aux(S,ListaAzioni,Visited,Limit)

idfs(MaxLimit,Step,Solution):-
	idfs_aux(1,MaxLimit,Step,Solution).

idfs_aux(Limit,_,_,Solution):-
	limited_depth_first_search(Limit,Solution),!.

idfs_aux(Limit,MaxLimit,Step,Solution):-
	Limit<MaxLimit,
	NewLimit is Limit+Step,
	idfs_aux(NewLimit,MaxLimit,Step,Solution).


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
