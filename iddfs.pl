% iterative deepening depth first search (ID DFS)
% optimal if action cost is 1 for every action

:- dynamic exp_nodes/1.

init_exp_nodes:-
    retractall(exp_nodes(_)),
    asserta(exp_nodes(0)).

inc_exp_nodes:- 
    exp_nodes(E),
    retractall(exp_nodes(_)),
    NewE is E + 1,
    asserta(exp_nodes(NewE)).

iddfs(MaxLimit,Step,Solution):-
    init_exp_nodes,
    initial(S),
    goal(G),
    write("\nInitial state:\n"), write(S), nl,
    write("\nGoal state:\n"), write(G), nl,
    statistics(walltime, []),
    iddfs_aux(1,MaxLimit,Step,Sol),
    statistics(walltime, [ _ | [ExecutionTime]]), % _ stand for NewTimeSinceStart
    reverse(Sol,Solution),
    write("\nStatistics:\n"),
    length(Solution,Depth),
    write("\nSolution depth: "), write(Depth),
    write('\nExecution took '), write(ExecutionTime), write(' ms.'),
    exp_nodes(Expanded_nodes),
    write('\nExpanded nodes: '), write(Expanded_nodes),
    write('\n\nSolution: '), write(Solution).


iddfs_aux(Limit,_,_,Solution):-
    limited_depth_first_search(Limit,Solution),!.

iddfs_aux(Limit,MaxLimit,Step,Solution):-
    Limit<MaxLimit,
    NewLimit is Limit+Step,
    iddfs_aux(NewLimit,MaxLimit,Step,Solution).


limited_depth_first_search(Limit,Solution):-
    initial(S),
    ldfs_aux(S,[S],Limit,Solution).


ldfs_aux(S,_,_,[]):-final(S).

ldfs_aux(S,Visited,Limit,[Action|ActionsTail]):-
    Limit > 0,
    applicable(Action,S),
    transform(Action,S,NewS),
    \+member(NewS,Visited),
    inc_exp_nodes,
    DecrementedLimit is Limit-1,
    ldfs_aux(NewS,[NewS|Visited],DecrementedLimit,ActionsTail).

