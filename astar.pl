bfs(Soluzione):-
    iniziale(S),
    bfs_aux([nodo(S,[])],[],Soluzione).

% bfs_aux(Coda,Visitati,Soluzione)
% Coda = [nodo(S,Azioni,G,H)|...]

bfs_aux([nodo(S,Azioni,G,H)|_],_,Azioni):-finale(S),!.
bfs_aux([nodo(S,Azioni,G,H)|Tail],Visitati,Soluzione):-
    findall(Azione,applicabile(Azione,S),ListaApplicabili),
    generaFigli(nodo(S,Azioni,G,H),ListaApplicabili,[S|Visitati],ListaFigli),

    %append(Tail,ListaFigli,NuovaCoda),
    %qui ord insert 
    bfs_aux(NuovaCoda,[S|Visitati],Soluzione).

generaFigli(_,[],_,[]).

generaFigli(nodo(S,AzioniPerS),[Azione|AltreAzioni],Visitati,[nodo(SNuovo,[Azione|AzioniPerS])|FigliTail]):-
    trasforma(Azione,S,SNuovo),
    \+member(SNuovo,Visitati),!,
    generaFigli(nodo(S,AzioniPerS),AltreAzioni,Visitati,FigliTail).

generaFigli(nodo(S,AzioniPerS),[_|AltreAzioni],Visitati,FigliTail):-
    generaFigli(nodo(S,AzioniPerS),AltreAzioni,Visitati,FigliTail).



ordInsert([],nodo(SP,AzioniP,GP,HP),[nodo(SP,AzioniP,GP,HP)]).

ordInsert([nodo(_,_,G,H)|Tail],nodo(SP,AzioniP,GP,HP),NuovaLista):-
    F is G + H,
    FP is GP + HP,
    F < FP,!,
    ordInsert(Tail,nodo(SP,AzioniP,GP,HP),NuovaLista).

ordInsert(Lista,nodo(SP,AzioniP,GP,HP),[nodo(SP,AzioniP,GP,HP)|Lista]).