bfs(Soluzione):-
    iniziale(S),
    bfs_aux([nodo(S,[])],[],Soluzione).

% bfs_aux(Coda,Visitati,Soluzione)
% Coda = [nodo(S,Azioni)|...]
bfs_aux([nodo(S,Azioni)|_],_,Azioni):-finale(S),!.
bfs_aux([nodo(S,Azioni)|Tail],Visitati,Soluzione):-
    findall(Azione,applicabile(Azione,S),ListaApplicabili),
    generaFigli(nodo(S,Azioni),ListaApplicabili,[S|Visitati],ListaFigli),
    append(Tail,ListaFigli,NuovaCoda),
    bfs_aux(NuovaCoda,[S|Visitati],Soluzione).

generaFigli(_,[],_,[]).
generaFigli(nodo(S,AzioniPerS),[Azione|AltreAzioni],Visitati,[nodo(SNuovo,[Azione|AzioniPerS])|FigliTail]):-
    trasforma(Azione,S,SNuovo),
    \+member(SNuovo,Visitati),!,
    generaFigli(nodo(S,AzioniPerS),AltreAzioni,Visitati,FigliTail).
generaFigli(nodo(S,AzioniPerS),[_|AltreAzioni],Visitati,FigliTail):-
    generaFigli(nodo(S,AzioniPerS),AltreAzioni,Visitati,FigliTail).




