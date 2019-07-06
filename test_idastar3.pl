% A test version of idastar that does not order children
% and therefore is not optimal

:- dynamic fmin/1.
:- dynamic exp_nodes/1.

infinity(9999999).

init_exp_nodes:-
    retractall(exp_nodes(_)),
    asserta(exp_nodes(0)).

inc_exp_nodes:- 
    exp_nodes(E),
    retractall(exp_nodes(_)),
    NewE is E + 1,
    asserta(exp_nodes(NewE)).
    
    
idastar(Solution):-
    init_exp_nodes,
    initial(S),
    goal(G),
    write("\nInitial state:\n"), write(S), nl,
    write("\nGoal state:\n"), write(G), nl,
    write("\nStatistics:\n"),
    statistics(walltime, []),
    heuristic(S,G,HRoot),
	idastar_aux(HRoot,Solution),
	statistics(walltime, [ _ | [ExecutionTime]]), % _ stands for NewTimeSinceStart
	%reverse(Sol,Solution), % no need to reverse since solution is built backward
    write('\nExecution took '), write(ExecutionTime), write(' ms.'),
    exp_nodes(Expanded_nodes),
    write('\nExpanded nodes: '), write(Expanded_nodes),
    write('\n\nSolution: '), write(Solution).

	
idastar_aux(FLimit,Solution):-
    retractall(fmin(_)),
    infinity(Infinity),
    asserta(fmin(Infinity)),
    ldfs(FLimit,Solution),!.

idastar_aux(_,Solution):-
    fmin(FMin),
    infinity(Infinity),
    FMin < Infinity,
    write("Retry with f limit:  "), write(FMin), nl,
    idastar_aux(FMin,Solution).


ldfs(FLimit,Solution):-
    initial(S),
    ldfs_aux(S,0,0,[S],FLimit,Solution).
  
  
ldfs_aux(S,G,F,_,FLimit,[]):-
    F =< FLimit,
    final(S),!,
    write("\nSolution cost: "), write(G).

ldfs_aux(S,G,F,Visited,FLimit,[Action|Solution]):-
    F =< FLimit,!,
    applicable(Action,S),
    transform(Action,S,NewS),
    \+member(NewS,Visited),
    inc_exp_nodes, %%%%%
    get_action_cost(Action,ActionCost),
    NewG is G + ActionCost,
    goal(Goal),
    heuristic(NewS,Goal,NewH),
    NewF is NewG + NewH,
    ldfs_aux(NewS,NewG,NewF,[NewS|Visited],FLimit,Solution).
  
ldfs_aux(_,_,F,_,FLimit,_):-
    F > FLimit,
    update_fmin(F),
    fail.


update_fmin(NewFMin):-
    fmin(OldFMin),
    NewFMin < OldFMin,!,
    retractall(fmin(_)),
    asserta(fmin(NewFMin)).

update_fmin(_).


get_action_cost(Action,Cost):-action_cost(Action,Cost).
get_action_cost(_,1).

