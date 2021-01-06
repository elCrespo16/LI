%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
% Primer apartado

subcjto([],[]).
subcjto([X|C],[X|S]):-subcjto(C,S).
subcjto([_|C],S):-subcjto(C,S).

cifras(L,N):- subcjto(L,S), permutacion(S,P), expresion(P,E), 
              N is E, write(E),nl,fail.                       

expresion([X],X).                                             
expresion(L,E1+E2):- concat(L1,L2,L),  L1\=[],L2\=[],         
                     expresion(L1,E1), expresion(L2,E2).      
expresion(L,E1-E2):- concat(L1,L2,L),  L1\=[],L2\=[],         
                     expresion(L1,E1), expresion(L2,E2).      
expresion(L,E1*E2):- concat(L1,L2,L),  L1\=[],L2\=[],         
                     expresion(L1,E1), expresion(L2,E2).      

expresion(L,E1//E2):- concat(L1,L2,L),  L1\=[],L2\=[],         
                     expresion(L1,E1), expresion(L2,E2),
         		     X is E2, X \= 0, 0 is E1 mod E2. 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
% Segundo apartado

% Esquema original
% camino( E,E, C,C ).

% camino( EstadoActual, EstadoFinal, CaminoHastaAhora, CaminoTotal ):-
%    unPaso( EstadoActual, EstSiguiente ),
%    \+member(EstSiguiente,CaminoHastaAhora),
%    camino( EstSiguiente, EstadoFinal, [EstSiguiente|CaminoHastaAhora], CaminoTotal ).

% solucionOptima:-
%    nat(N), % Buscamos soluci´on de "coste" 0; si no, de 1, etc.
%    camino([0,0],[0,4],[[0,0]],C), % En "hacer aguas": -un estado es [cubo5,cubo8], y
%    length(C,N), % -el coste es la longitud de C.
%    write(C).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%% CANIBALES LOKO
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


solucionOptimaCanibales:- % Un estado es una lista que contiene dos listas dentro, la primera lista contiene las personas en un lado del rio [[Misioneros,Canibales],[Misioneros,Canibales],LadoCanoa]
    between(0,40,N), % Buscamos soluci´on de "coste" 0; si no, de 1, etc.
    write(N), nl,
    caminoCan([[3,3],[0,0],0],[[0,0],[3,3],1],[[[3,3],[0,0],0]],C), 
    length(C,N), % -el coste es la longitud de C.
    write(C).

caminoCan( E,E, C,C ).

caminoCan( EstadoActual, EstadoFinal, CaminoHastaAhora, CaminoTotal ):-
    unPasoCan( EstadoActual, EstSiguiente ),
    not(member(EstSiguiente,CaminoHastaAhora)),
    caminoCan( EstSiguiente, EstadoFinal, [EstSiguiente|CaminoHastaAhora], CaminoTotal ).

we_good([M,C]):- M >= C.
we_good([M,_]):- M is 0.

% Mover un misionero de izquierda a derecha
unPasoCan([[MisionerosIzq,CanibalesIzq],[MisionerosDer,CanibalesDer],LadoCanoa],[[MisionerosIzqRes,CanibalesIzqRes],[MisionerosDerRes,CanibalesDerRes],LadoCanoaRes]):-
    LadoCanoa = 0, % Canoa en la izquierda
    MisionerosIzq > 0,
    MisionerosIzqRes is MisionerosIzq - 1,
    MisionerosDerRes is MisionerosDer + 1,
    CanibalesDerRes is CanibalesDer,
    CanibalesIzqRes is CanibalesIzq,
    we_good([MisionerosIzqRes,CanibalesIzqRes]),
    we_good([MisionerosDerRes,CanibalesDerRes]),
    LadoCanoaRes is 1.


% Mover dos misionero de izquierda a derecha
unPasoCan([[MisionerosIzq,CanibalesIzq],[MisionerosDer,CanibalesDer],LadoCanoa],[[MisionerosIzqRes,CanibalesIzqRes],[MisionerosDerRes,CanibalesDerRes],LadoCanoaRes]):-
    LadoCanoa = 0,
    MisionerosIzq > 1,
    MisionerosIzqRes is MisionerosIzq - 2,
    MisionerosDerRes is MisionerosDer + 2,
    CanibalesDerRes is CanibalesDer,
    CanibalesIzqRes is CanibalesIzq,
    we_good([MisionerosIzqRes,CanibalesIzqRes]),
    we_good([MisionerosDerRes,CanibalesDerRes]),
    LadoCanoaRes is 1.

% Mover un canibal de izquierda a derecha
unPasoCan([[MisionerosIzq,CanibalesIzq],[MisionerosDer,CanibalesDer],LadoCanoa],[[MisionerosIzqRes,CanibalesIzqRes],[MisionerosDerRes,CanibalesDerRes],LadoCanoaRes]):-
    LadoCanoa = 0,
    CanibalesIzq > 0,
    CanibalesIzqRes is CanibalesIzq - 1,
    CanibalesDerRes is CanibalesDer + 1,
    MisionerosDerRes is MisionerosDer,
    MisionerosIzqRes is MisionerosIzq,
    we_good([MisionerosIzqRes,CanibalesIzqRes]),
    we_good([MisionerosDerRes,CanibalesDerRes]),
    LadoCanoaRes is 1.

% Mover dos canibal de izquierda a derecha
unPasoCan([[MisionerosIzq,CanibalesIzq],[MisionerosDer,CanibalesDer],LadoCanoa],[[MisionerosIzqRes,CanibalesIzqRes],[MisionerosDerRes,CanibalesDerRes],LadoCanoaRes]):-
    LadoCanoa = 0,
    CanibalesIzq > 1,
    CanibalesIzqRes is CanibalesIzq - 2,
    CanibalesDerRes is CanibalesDer + 2,
    MisionerosDerRes is MisionerosDer,
    MisionerosIzqRes is MisionerosIzq,
    we_good([MisionerosIzqRes,CanibalesIzqRes]),
    we_good([MisionerosDerRes,CanibalesDerRes]),
    LadoCanoaRes is 1.

% Mover un canibal y un misionero de izquierda a derecha
unPasoCan([[MisionerosIzq,CanibalesIzq],[MisionerosDer,CanibalesDer],LadoCanoa],[[MisionerosIzqRes,CanibalesIzqRes],[MisionerosDerRes,CanibalesDerRes],LadoCanoaRes]):-
    LadoCanoa = 0, % Canoa en la izquierda
    MisionerosIzq > 0,
    MisionerosIzqRes is MisionerosIzq - 1,
    MisionerosDerRes is MisionerosDer + 1,
    CanibalesIzq > 0,
    CanibalesIzqRes is CanibalesIzq - 1,
    CanibalesDerRes is CanibalesDer + 1,
    we_good([MisionerosIzqRes,CanibalesIzqRes]),
    we_good([MisionerosDerRes,CanibalesDerRes]),
    LadoCanoaRes is 1.




% Mover un misionero de derecha a izquierda
unPasoCan([[MisionerosIzq,CanibalesIzq],[MisionerosDer,CanibalesDer],LadoCanoa],[[MisionerosIzqRes,CanibalesIzqRes],[MisionerosDerRes,CanibalesDerRes],LadoCanoaRes]):-
    LadoCanoa = 1, % Canoa en la derecha
    MisionerosDer > 0,
    MisionerosDerRes is MisionerosDer - 1,
    MisionerosIzqRes is MisionerosIzq + 1,
    CanibalesDerRes is CanibalesDer,
    CanibalesIzqRes is CanibalesIzq,
    we_good([MisionerosIzqRes,CanibalesIzqRes]),
    we_good([MisionerosDerRes,CanibalesDerRes]),
    LadoCanoaRes is 0.

% Mover dos misionero de derecha a izquierda
unPasoCan([[MisionerosIzq,CanibalesIzq],[MisionerosDer,CanibalesDer],LadoCanoa],[[MisionerosIzqRes,CanibalesIzqRes],[MisionerosDerRes,CanibalesDerRes],LadoCanoaRes]):-
    LadoCanoa = 1, % Canoa en la derecha
    MisionerosDer > 1,
    MisionerosDerRes is MisionerosDer - 2,
    MisionerosIzqRes is MisionerosIzq + 2,
    CanibalesDerRes is CanibalesDer,
    CanibalesIzqRes is CanibalesIzq,
    we_good([MisionerosIzqRes,CanibalesIzqRes]),
    we_good([MisionerosDerRes,CanibalesDerRes]),
    LadoCanoaRes is 0.

% Mover un canibal de derecha a izquierda
unPasoCan([[MisionerosIzq,CanibalesIzq],[MisionerosDer,CanibalesDer],LadoCanoa],[[MisionerosIzqRes,CanibalesIzqRes],[MisionerosDerRes,CanibalesDerRes],LadoCanoaRes]):-
    LadoCanoa = 1,
    CanibalesDer > 0,
    CanibalesDerRes is CanibalesDer - 1,
    CanibalesIzqRes is CanibalesIzq + 1,
    MisionerosDerRes is MisionerosDer,
    MisionerosIzqRes is MisionerosIzq,
    we_good([MisionerosIzqRes,CanibalesIzqRes]),
    we_good([MisionerosDerRes,CanibalesDerRes]),
    LadoCanoaRes is 0.

% Mover dos canibal de derecha a izquierda
unPasoCan([[MisionerosIzq,CanibalesIzq],[MisionerosDer,CanibalesDer],LadoCanoa],[[MisionerosIzqRes,CanibalesIzqRes],[MisionerosDerRes,CanibalesDerRes],LadoCanoaRes]):-
    LadoCanoa = 1,
    CanibalesDer > 1,
    CanibalesDerRes is CanibalesDer - 2,
    CanibalesIzqRes is CanibalesIzq + 2,
    MisionerosDerRes is MisionerosDer,
    MisionerosIzqRes is MisionerosIzq,
    we_good([MisionerosIzqRes,CanibalesIzqRes]),
    we_good([MisionerosDerRes,CanibalesDerRes]),
    LadoCanoaRes is 0.

% Mover un canibal y un misionero de derecha a izquierda
unPasoCan([[MisionerosIzq,CanibalesIzq],[MisionerosDer,CanibalesDer],LadoCanoa],[[MisionerosIzqRes,CanibalesIzqRes],[MisionerosDerRes,CanibalesDerRes],LadoCanoaRes]):-
    LadoCanoa = 1, % Canoa en la derecha
    MisionerosDer > 0,
    MisionerosDerRes is MisionerosDer - 1,
    MisionerosIzqRes is MisionerosIzq + 1,
    CanibalesDer > 0,
    CanibalesDerRes is CanibalesDer - 1,
    CanibalesIzqRes is CanibalesIzq + 1,
    we_good([MisionerosIzqRes,CanibalesIzqRes]),
    we_good([MisionerosDerRes,CanibalesDerRes]),
    LadoCanoaRes is 0.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%% CABALLITO LOKO
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

solucionOptimaCaballo(N):- % Un estado es una lista que contiene una lista dentro, contiene la posicion actual y el tamaño del tablero [[FilaAct,ColAct],Tamaño]
    between(0,40,P), % Buscamos soluci´on de "coste" 0; si no, de 1, etc.
    write(P), nl,
    caminoCab([[],N],[[],N],[[[],N]],C), % Probar posiciones iniciales y finales
    length(C,P), % -el coste es la longitud de C.
    write(C).

caminoCab( E,E, C,C ).

caminoCab( EstadoActual, EstadoFinal, CaminoHastaAhora, CaminoTotal ):-
    unPasoCab( EstadoActual, EstSiguiente ),
    not(member(EstSiguiente,CaminoHastaAhora)),
    caminoCab( EstSiguiente, EstadoFinal, [EstSiguiente|CaminoHastaAhora], CaminoTotal ).

posOk([Fila,Col],N):-
    Fila >= 0,
    Fila < N,
    Col >= 0,
    Col < N.

% Arriba izquierda
unPasoCab([[FilaAct,ColAct],N],[[FilaRes,ColRes],N]):-
    FilaRes is FilaAct - 2,
    ColRes is ColAct - 1,
    posOk([FilaRes,ColRes],N).

% Arriba derecha
unPasoCab([[FilaAct,ColAct],N],[[FilaRes,ColRes],N]):-
    FilaRes is FilaAct - 2,
    ColRes is ColAct + 1,
    posOk([FilaRes,ColRes],N).

% izquierda arriba
unPasoCab([[FilaAct,ColAct],N],[[FilaRes,ColRes],N]):-
    FilaRes is FilaAct - 1,
    ColRes is ColAct - 2,
    posOk([FilaRes,ColRes],N).
    
% izquierda abajo
unPasoCab([[FilaAct,ColAct],N],[[FilaRes,ColRes],N]):-
    FilaRes is FilaAct + 1,
    ColRes is ColAct - 2,
    posOk([FilaRes,ColRes],N).

% derecha arriba
unPasoCab([[FilaAct,ColAct],N],[[FilaRes,ColRes],N]):-
    FilaRes is FilaAct - 1,
    ColRes is ColAct + 2,
    posOk([FilaRes,ColRes],N).

% derecha abajo
unPasoCab([[FilaAct,ColAct],N],[[FilaRes,ColRes],N]):-
    FilaRes is FilaAct + 1,
    ColRes is ColAct + 2,
    posOk([FilaRes,ColRes],N).
    
% abajo izquierda
unPasoCab([[FilaAct,ColAct],N],[[FilaRes,ColRes],N]):-
    FilaRes is FilaAct + 2,
    ColRes is ColAct - 1,
    posOk([FilaRes,ColRes],N).

% abajo derecha
unPasoCab([[FilaAct,ColAct],N],[[FilaRes,ColRes],N]):-
    FilaRes is FilaAct + 2,
    ColRes is ColAct + 1,
    posOk([FilaRes,ColRes],N).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%% GORDITO LOKO
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

nat(0).
nat(N):- nat(X), N is X+1.

% combinaciones de N elementos de una lista
comb(0, T, T, []):- !.
comb(N, [X|T], CA, [X|CO]):- N1 is N-1, comb(N1, T, CA, CO).
comb(N, [X|T], [X|CA], CO):- comb(N, T, CA, CO).

% une dos listas, manteniendo el orden
merge([], C, C).
merge(B, [], B).
merge([X|XS], [Y|YS], [X|C]):- X=<Y, merge(XS, [Y|YS], C).
merge([X|XS], [Y|YS], [Y|C]):- X>Y, merge([X|XS], YS, C).

% maximo elemento de una lista
listMax([X], X).
listMax([X|XS], N):- listMax(XS, M), N is max(M, X).

% envia 1 o 2 personas de un lado al otro del puente
% las personas salen del lado D1 para ir al I1, con coste C1
cruza(I1-D1-C1, I2-D2-C2):-
  length(D1, N), N1 is min(N, 2),           % que crucen maximo 2 personas
  between(1, N1, A), comb(A, D1, D2, CO),   % combinaciones de A elementos de D1
  merge(I1, CO, I2),                        % anadir a I1 los que cruzan
  listMax(CO, M), C2 is C1+M.               % sumar el coste de cruzar

% existen 2 tipos de paso:
% cruzar de un lado al otro, o del otro al uno
unPaso(a-I1-D1-C1, b-I2-D2-C2):- cruza(I1-D1-C1, I2-D2-C2).
unPaso(b-I1-D1-C1, a-I2-D2-C2):- cruza(D1-I1-C1, D2-I2-C2).

% podamos estados con coste superior a N
poda(_-_-_-C, N):- N>C.

camino(E, E, C, C, _).
camino(EstadoActual, EstadoFinal, CaminoHastaAhora, CaminoTotal, N):-
  poda(EstadoActual, N),
  unPaso(EstadoActual, EstSiguiente),
  \+member(EstSiguiente, CaminoHastaAhora),
  camino(EstSiguiente, EstadoFinal, [EstSiguiente|CaminoHastaAhora], CaminoTotal, N).

solucionOptima:- nat(N), 
  camino(a-[]-[1,2,5,8]-0, b-[1,2,5,8]-[]-N, [a-[]-[1,2,5,8]-0], C, N),
  muestraSolucion(N, C).

muestraSolucion(N, C):-
  write("Solucion de coste "), write(N), nl, 
  displaySol(C, _).
  
displaySol([], -1).
displaySol([X|XS], N):- displaySol(XS, NI), 
  N is NI+1, write(N), write(" - "), write(X), nl.
  
main:- solucionOptima, halt.