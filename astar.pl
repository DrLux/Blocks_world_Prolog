astar(Soluzione):-
    iniziale(S),
    astar_aux([nodo(S,[],0,0)],[],Soluzione).

% astar_aux(Coda,Visitati,Soluzione)
% Coda = [nodo(S,Azioni,G,H)|...]

astar_aux([nodo(S,Azioni,_,_)|_],_,Azioni):-finale(S),!.

astar_aux([nodo(S,Azioni,G,H)|PriorityQueue],Visitati,Soluzione):-
    findall(Azione,applicabile(Azione,S),ListaApplicabili),
    expandChildren(nodo(S,Azioni,G,H),ListaApplicabili,PriorityQueue,[S|Visitati],PriorityQueueUpdated,VisitatiUpdated),
    astar_aux(PriorityQueueUpdated,VisitatiUpdated,Soluzione).

expandChildren(_,[],PriorityQueue,Visitati,PriorityQueue,Visitati).
    
expandChildren(_,[],[],Visitati,[],Visitati).

%When node is already in PriorityQueue 
expandChildren(nodo(S,AzioniPerS,G,H),[AzioneApplicabile|AltreAzioni],PriorityQueue,Visitati,PriorityQueueUpdated,VisitatiUpdated):-
    trasforma(AzioneApplicabile,S,SNuovo,ActionCost),
    member(nodo(SNuovo,OldAzioni,OldG,OldH),PriorityQueue),!,
    NewG is G + ActionCost,
    insertInPriorityQueue(nodo(SNuovo,OldAzioni,OldG,OldH),nodo(SNuovo,[AzioneApplicabile|AzioniPerS],NewG,OldH),PriorityQueue,NewPriorityQueue),
    expandChildren(nodo(S,AzioniPerS,G,H),AltreAzioni,NewPriorityQueue,Visitati,PriorityQueueUpdated,VisitatiUpdated).


%When node is already in Visitati
expandChildren(nodo(S,AzioniPerS,G,H),[AzioneApplicabile|AltreAzioni],PriorityQueue,Visitati,PriorityQueueUpdated,VisitatiUpdated):-
    trasforma(AzioneApplicabile,S,SNuovo,ActionCost),
    %write(Visitati),
    member(nodo(SNuovo,OldAzioni,OldG,OldH),Visitati),!,
    NewG is G + ActionCost,
    insertInVisitati(nodo(SNuovo,OldAzioni,OldG,OldH),nodo(SNuovo,[AzioneApplicabile|AzioniPerS],NewG,OldH),Visitati,NewVisitati),
    expandChildren(nodo(S,AzioniPerS,G,H),AltreAzioni,PriorityQueue,NewVisitati,PriorityQueueUpdated,VisitatiUpdated).


%When node is not in PriorityQueue and nor in Visitati
expandChildren(nodo(S,AzioniPerS,G,H),[AzioneApplicabile|AltreAzioni],PriorityQueue,Visitati,PriorityQueueUpdated,VisitatiUpdated):-
    trasforma(AzioneApplicabile,S,SNuovo,ActionCost),
    NewG is G + ActionCost,
    goal(Goal),
    heuristic(SNuovo,Goal,NewH),
    write(PriorityQueue),
    ordInsert(PriorityQueue,nodo(SNuovo,[AzioneApplicabile|AzioniPerS],NewG,NewH),NewPriorityQueue),
    expandChildren(nodo(S,AzioniPerS,G,H),AltreAzioni,NewPriorityQueue,Visitati,PriorityQueueUpdated,VisitatiUpdated).

insertInPriorityQueue(nodo(SNuovo,OldAzioni,OldG,OldH),nodo(SNuovo,NewAzioni,NewG,_),PriorityQueue,NewPriorityQueue):-
    write('NewG '),
    write(NewG),
    write(' OldG '),
    write(OldG),
    NewG < OldG,!,
    delete(PriorityQueue,nodo(SNuovo,OldAzioni,OldG,OldH),PQR),
    ordInsert(PQR,nodo(SNuovo,NewAzioni,NewG,OldH),NewPriorityQueue).

insertInPriorityQueue(_,_,PriorityQueue,PriorityQueue).

insertInVisitati(nodo(SNuovo,OldAzioni,OldG,OldH),nodo(SNuovo,NewAzioni,NewG,OldH),Visitati,NewVisitati):-
    write('NewG '),
    write(NewG),
    write(' OldG '),
    write(OldG),
    NewG < OldG,!,
    delete(Visitati,nodo(SNuovo,OldAzioni,OldG,OldH),VR),
    NewVisitati = [nodo(SNuovo,NewAzioni,NewG,OldH)|VR].

insertInVisitati(_,_,Visitati,Visitati).


%ordInsert([],nodo(SP,AzioniP,GP,HP),[nodo(SP,AzioniP,GP,HP)]):-write("posso?").
ordInsert([],X,[X]).

ordInsert([nodo(_,_,G,H)|Tail],nodo(SP,AzioniP,GP,HP),NuovaLista):-
    F is G + H,
    FP is GP + HP,
    F < FP,!,
    ordInsert(Tail,nodo(SP,AzioniP,GP,HP),NuovaLista).

ordInsert(Lista,nodo(SP,AzioniP,GP,HP),[nodo(SP,AzioniP,GP,HP)|Lista]).