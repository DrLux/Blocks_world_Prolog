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
	
idastar_aux(FLimit,Solution):-
    retractall(fmin(_)),
    asserta(fmin(999999)),
    ldfs(FLimit,Solution).

idastar_aux(_,Solution):-
    fmin(FMin),
    FMin < 999999,
    write("retry:  "),
    write(FMin),nl,
    idastar_aux(FMin,Solution).
    
ldfs(FLimit,Solution):-
    initial(S),
    ldfs_aux(S,0,0,[S],FLimit,Solution).
    
ldfs_aux(S,_,F,_,FLimit,[]):-
    F =< FLimit,
    final(S).

ldfs_aux(S,G,F,Visited,FLimit,[Action|Solution]):-
    F =< FLimit,
    applicable(Action,S),
    transform(Action,S,NewS),
    \+member(NewS,Visited),
    inc_exp_nodes,
    getActionCost(Action,ActionCost),
    NewG is G + ActionCost,
    goal(Goal),
    heuristic(NewS,Goal,NewH),
    NewF is NewG + NewH,
    ldfs_aux(NewS,NewG,NewF,[NewS|Visited],FLimit,Solution).
  
ldfs_aux(_,_,F,_,FLimit,_):-
    F > FLimit,
    updateFMin(F),
    fail.

updateFMin(NewFMin):-
    fmin(OldFMin),
    NewFMin < OldFMin,!,
    retractall(fmin(_)),
    asserta(fmin(NewFMin)).

updateFMin(_).

getActionCost(Action,Cost):-actionCost(Action,Cost).
getActionCost(_,1).

