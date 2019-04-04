% nth(Lista,Posizione,Valore)

nth([Head|_],0,Head):-!.
nth([_|Tail],Pos,X):-
  nonvar(Pos),!,
  Pos1 is Pos-1,
  nth(Tail,Pos1,X).
nth([_|Tail],Pos,X):-
  nth(Tail,Pos1,X),
  Pos is Pos1+1.


% swap(Lista,Pos1,Pos2,NuovaLista)

swap(Lista,Pos1,Pos2,NuovaLista):-
  nth(Lista,Pos1,X1),
  nth(Lista,Pos2,X2),
  setElement(Lista,Pos2,X1,Temp),
  setElement(Temp,Pos1,X2,NuovaLista).

% setElement(Lista,Posizione,Valore,NuovaLista)

setElement([_|Tail],0,X,[X|Tail]):-!.
setElement([Head|Tail],Pos,X,[Head|NuovaTail]):-
  Pos1 is Pos-1,
  setElement(Tail,Pos1,X,NuovaTail).
