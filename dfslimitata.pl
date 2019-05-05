
:- dynamic exp_nodes/1.

init_exp_nodes :-
    retractall(exp_nodes(_)),
    asserta(exp_nodes(0)).

inc_exp_nodes :- 
    exp_nodes(E),
    retractall(exp_nodes(_)),
    NewE is E + 1,
    asserta(exp_nodes(NewE)).

idfs(MaxLimit,Step,Solution):-
    init_exp_nodes,
    statistics(walltime, []),
    initial(S),
    write("\nstato initial:\n"),write(S),nl,
    write("\nstato final:\n"),
    goal(G),
    write(G),nl,
    write("\nStatistics:\n"),
    idfs_aux(1,MaxLimit,Step,Sol),
    reverse(Sol,Solution),
    length(Solution, Cost),
    write("\nCost Solution: "),
    write(Cost),
    statistics(walltime, [ _ | [ExecutionTime]]), %_ stand for NewTimeSinceStart
    write('\nExecution took '), write(ExecutionTime), write(' ms.'),
    exp_nodes(Expanded_nodes),
    write('\nExpanded nodes: '), write(Expanded_nodes),
    write('\n\nSolution: '), write(Solution).

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
    transform(Action,S,NewS),
    \+member(NewS,Visited),
    inc_exp_nodes,
    DecrementedLimit is Limit-1,
    ldfs_aux(NewS,[NewS|Visited],DecrementedLimit,ActionsTail).
    
    
    
