% dfs_aux(S,ListaAzioni,Visitati,Soglia)

idfs(Soluzione,SogliaMax,Step):-
	idfs_aux(Soluzione,1,SogliaMax,Step).

idfs_aux(Soluzione,Soglia,_,_):-
	depth_limit_search(Soluzione,Soglia),!.

idfs_aux(Soluzione,Soglia,SogliaMax,Step):-
	Soglia<SogliaMax,
	Sogliap is Soglia+Step,
	idfs_aux(Soluzione,Sogliap,SogliaMax,Step).


depth_limit_search(Soluzione,Soglia):-
    iniziale(S),
    dfs_aux(S,Soluzione,[S],Soglia).

dfs_aux(S,[],_,_):-finale(S).
dfs_aux(S,[Azione|AzioniTail],Visitati,Soglia):-
    Soglia>0,
    applicabile(Azione,S),
    trasforma(Azione,S,SNuovo),
    \+member(SNuovo,Visitati),
    NuovaSoglia is Soglia-1,
    dfs_aux(SNuovo,AzioniTail,[SNuovo|Visitati],NuovaSoglia).
