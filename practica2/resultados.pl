concatN([],L,L).
concatN([X|L1],L2,[X|L3]) :- concatN(L1,L2,L3).

subcjto([],[]).
subcjto([X|C],[X|S]):- subcjto(C,S).
subcjto([_|C],S):- subcjto(C,S).

pert(X,[X|_]).
pert(X,[_|L]):- pert(X,L).

long([],0).
long([_|L],M):- long(L,N), M is N + 1.

pert_con_resto(X,L,Resto):- concatN(L1,[X|L2],L), concatN(L1,L2,Resto).

permutacion([],[]).
permutacion(L,[X|P]):- pert_con_resto(X,L,R), permutacion(R,P).

% Ejercicio 2
% prod(L,P) P es el producto de los elementos de la lista de enteros dada L

prod([X],X).
prod([X|L],P):- prod(L,P1), P is X * P1.

% Ejercicio 3
% pescalar(L1,L2,P) P es el producto escalar de los vectores L1 y L2.

pescalar([X],[Y],X*Y).
pescalar([X|L1],[Y|L2],P):- pescalar(L1,L2,P1), P is P1 + X * Y.  

% Ejercicio 4
% union(L1,L2,P) P es la union de los L1, L2.
% intersec(L1,L2,P) P es la interseccion de L1 y L2,

union([],L,L).
union([X|L1],L2,P):- member(X,L2), !, union(L1,L2,P).
union([X|L1],L2,P):- union(L1,L2,P1), P = [X|P1].

intersec([],_,[]).
intersec([X|L1],L2,P):- member(X,L2), !, intersec(L1,L2,L3), P = [X|L3].
intersec([_|L1],L2,P):- intersec(L1,L2,P).

% Ejercicio 5
% lastOfList(L,X) X es el ultimo elemento de la lista
% inverse(L,R) R es L invertida
% ultimo de una lista sin usar concat
% lastOfList([X],X).
% lastOfList([_|L],X):- lastOfList(L,X1), X = X1.

ultimo(L,X):- concatN(_,[X],L). 

inverse([X],[X]):-!.
inverse(L,R):- concatN(L1,[X],L), !, inverse(L1,R1), concatN([X],R1,R).

% Ejercicio 6
% fib(N,R) R es el fibonacci de N

fib(1,1).
fib(2,1).
fib(N,R):- N > 2, N1 is N-1, N2 is N-2, fib(N1,R1), fib(N2,R2), R is R1 + R2.

% Ejercicio 7
% dados(P,N,L) L es la lista de maneras de conseguir P puntos tirando N dados

dados(P,1,L):- P < 7, L = [P], !.
dados(P,1,L):- P > 6, L = [], !.
dados(P,N,L):- D = [1,2,3,4,5,6], pert(X,D), X < P, P1 is P - X, N1 is N - 1, dados(P1,N1,L1), concatN(L1,[X],L).

% Ejercicio 8
% suma_demas(L) devuelve true si alguno de sus elementos es igual a la suma del resto.

% sum(L,N) N es la suma de los elementos de la lista L.

sum([X],X).
sum([X|L],R):- sum(L,R1), R is R1 + X.

suma_demas(L):- sum(L,N), pert(X,L), X is N - X, !.

% Ejercicio 9
% suma_ants(L) devuelve true si algun elemento es igual a la suma de sus anteriores en la lista L.

suma_ants(L):- concatN(L1,[X|_],L), sum(L1,N), N = X, !.

% Ejercicio 10
% card(L,R) R es una lista con las aparciones de cada elemento de L en forma de pares

% apar(N,L,R) R es el numero de apariciones de N en L

apar(_,[],0).
apar(N,[X|L],R):- N = X, !, apar(N,L,R1), R is R1 + 1.
apar(N,[_|L],R):- apar(N,L,R).

% without(L,N,R) R es L sin las aparciones de N.
without(_,[],[]).
without(X,[X|L],R):- without(X,L,R), !.
without(X,[A|L],R):- X \= A, without(X,L,R1), concatN([A],R1,R).

card([],[]).
card([X|L],[[X,A]|R]):- apar(X,[X|L],A), without(X,L,Resto), card(Resto,R). 

% Ejercicio 11
% esta_ord(L) devuelve true si la lista esta ordenada crecientemente.

esta_ord([_]):- true.
esta_ord([X|L]):- concatN([X1],_,L), X1 < X, !, false.
esta_ord([_|L]):- esta_ord(L).

% Ejercicio 12
% ordenacion(L1,L2) L2 es L1 ordenada crecientemente.

ordenacion([],[]).
ordenacion(L1,L2):- permutacion(L1,L2), esta_ord(L2), !.

% Ejercicio 13: En el peor caso, se generan todas las permutaciones que son n!, siendo n el tamaño del vector y comprobar que esta
% ordenada tiene coste lineal, por eso el coste es O((n+1)!) 

% Ejercicio 14

% ordenacion2(L1,L2) L2 es la lista L1 ordenada usando el metodo de insercion
% insercion(X,L1,L2) L2 es la lista obtenida al insertar X en L1 en el sitio correspondiente

insercion(X,[],[X]):- !.
insercion(X,[A|L],R):- X > A, !,concatN([X],[A|L],R).
insercion(X,[A|L],R):- insercion(X,L,R1), concatN([A],R1,R).

ordenacion2([X],[X]).
ordenacion2([A|L1],L2):- ordenacion2(L1,R1), insercion(A,R1,L2).

% Ejercicio 15: En el caso peor, que es que la lista este ordenada de forma contraria al orden de ordenacion,
% la insercion debe comparar cada numero con el resto, eso es coste lineal, y debe repetir ese proceso por cada
% elemento, por eso el coste es O(N²).

% Ejercicio 16

mezcla([],[],[]):- !.
mezcla(L,[],L):- !.
mezcla([],L,L):- !.
mezcla([A],[B],R):- A > B, !, concatN([A],[B],R).
mezcla([A],[B],R):- A =< B, !, concatN([B],[A],R).
mezcla([A|L1],[B|L2],R):- A > B, !, mezcla(L1,[B|L2],R2), concatN([A],R2,R).
mezcla([A|L1],[B|L2],R):- A =< B, mezcla([A|L1],L2,R2), concatN([B],R2,R).

ordenacion3([],[]):- !.
ordenacion3([X],[X]):- !.
ordenacion3(L1,L2):- append(P1,P2,L1), long(P1,N), N > 0 ,long(P2,N2), !, N2 > 0, ordenacion3(P1,R1), ordenacion3(P2,R2), mezcla(R1,R2,L2).

% Ejercicio 17

% printNConcat(N,L,R) R es la lista obtenida al concatenar N con cada elemento de L
printNConcat(N,[L],[R]):- atom_concat(N,L,R), !.
printNConcat(N,[X|L], R):- printNConcat(N,L,R2), printNConcat(N,[X],R1), concatN(R1,R2,R).

% forEinL(L1,L2,R) R es la lista obtenida de concatenar cada elemento de L1 con cada elemento de L2
forEinL([X],L,R):- printNConcat(X,L,R), !.
forEinL([X|L],N1,R):- printNConcat(X,N1,R1), forEinL(L,N1,R2), concatN(R1,R2,R).

% prodCart(L,N,R) R es la lista obtenida de concatenar cada elemento de L con cada elemento de L, N veces
prodCart(L,1,L):- !.
prodCart(L,N,R):- N1 is N - 1, prodCart(L,N1,R1), forEinL(L,R1,R).

% printDict(A) escribe por consola la lista de palabras A

printDict(A):- A = [], !.
printDict(A):- atomic(A), !, write(A), nl.
printDict([R|L]):- printDict(R), printDict(L).

diccionario(A,N):- prodCart(A,N,R), printDict(R).

% Ejercicio 18
% palindromos(L) R es la lista de las combinaciones de L que son palindromos

es_palindromo(X):- inverse(X, R), X = R.

palindromos(L):- setof(R1,(permutacion(L,R1), es_palindromo(R1)),S),write(S).

% Ejercicio 19

% tenPowX(X,N) N es 10 elevado a X

tenPowX(1,10):- !.
tenPowX(X,N):- X1 is X - 1, tenPowX(X1,N1), N is N1 * 10.

% numberEscale(L,X) X es la lista L entendida como un unico entero

numberEscale([X],X):- !.
numberEscale([A|L],X):- long(L,N), numberEscale(L,R1), tenPowX(N,P), X is R1 + A * P.

%[S,E,N,D,M,O,R,Y] [S,E,N,D], [M,O,R,E],[M,O,N,E,Y]

sendMoney():- P = [S,E,N,D,M,O,R,Y],
                pert_con_resto(S,[1,2,3,4,5,6,7,8,9,0],L1),
                pert_con_resto(E,L1,L2),
                pert_con_resto(N,L2,L3),
                pert_con_resto(D,L3,L4),
                pert_con_resto(M,L4,L5),
                pert_con_resto(O,L5,L6),
                pert_con_resto(R,L6,L7),
                pert_con_resto(Y,L7,_),
                numberEscale([S,E,N,D], R1),
                numberEscale([M,O,R,E], R2),
                numberEscale([M,O,N,E,Y], R3),
                R3 is R1 + R2, 
                !,
                write(P), nl, write([S,E,N,D]),nl, write([M,O,R,E]),nl,write([M,O,N,E,Y]).

% Ejercicio 20 (Es igual al que habeis puesto como ejemplo porque no sabia como hacerlo)

simplifica(E,E1):- unpaso(E,E2),!, simplifica(E2,E1).
simplifica(E,E).

unpaso(A+B,A+C):- unpaso(B,C),!.
unpaso(B+A,C+A):- unpaso(B,C),!.
unpaso(A*B,A*C):- unpaso(B,C),!.
unpaso(B*A,C*A):- unpaso(B,C),!.
unpaso(0*_,0):-!.
unpaso(_*0,0):-!.
unpaso(1*X,X):-!.
unpaso(X*1,X):-!.
unpaso(0+X,X):-!.
unpaso(X+0,X):-!.
unpaso(N1+N2,N3):- number(N1), number(N2), N3 is N1+N2,!.
unpaso(N1*N2,N3):- number(N1), number(N2), N3 is N1*N2,!.
unpaso(N1*X+N2*X,N3*X):- number(N1), number(N2), N3 is N1+N2,!.
unpaso(N1*X+X*N2,N3*X):- number(N1), number(N2), N3 is N1+N2,!.
unpaso(X*N1+N2*X,N3*X):- number(N1), number(N2), N3 is N1+N2,!.
unpaso(X*N1+X*N2,N3*X):- number(N1), number(N2), N3 is N1+N2,!.

% Ejercicio 21 

we_good([M,C]):- M >= C.
we_good([M,_]):- M is 0.

% Muevo 1 misionero de Izquierda a derecha
move([M1,C1],[M2,C2],[M3,C1],[M4,C2],0,1):- M1 > 0, M3 is M1 - 1, M4 is M2 +1, we_good([M3,C1]), we_good([M4,C2]).
% Muevo 2 misionero de Izquierda a derecha
move([M1,C1],[M2,C2],[M3,C1],[M4,C2],0,1):- M1 > 1, M3 is M1 - 2, M4 is M2 +2, we_good([M3,C1]), we_good([M4,C2]).
% Muevo 1 canibal de Izquierda a derecha
move([M1,C1],[M2,C2],[M1,C3],[M2,C4],0,1):- C1 > 0, C3 is C1 - 1, C4 is C2 +1, we_good([M1,C3]), we_good([M2,C4]).
% Muevo 2 canibal de Izquierda a derecha
move([M1,C1],[M2,C2],[M1,C3],[M2,C4],0,1):- C1 > 1, C3 is C1 - 2, C4 is C2 +2, we_good([M1,C3]), we_good([M2,C4]).
% Muevo 1 canibal y 1 misionero de Izquierda a derecha
move([M1,C1],[M2,C2],[M3,C3],[M4,C4],0,1):- M1 > 0, C1 > 0,M3 is M1 - 1, M4 is M2 +1, C3 is C1 - 1, C4 is C2 +1,
                                            we_good([M3,C3]), we_good([M4,C4]).

% Muevo 1 misionero de derecha a izquierda
move([M1,C1],[M2,C2],[M3,C1],[M4,C2],1,0):- M2 > 0, M3 is M1 + 1, M4 is M2 -1, we_good([M3,C1]), we_good([M4,C2]).
% Muevo 2 misionero de derecha a izquierda
move([M1,C1],[M2,C2],[M3,C1],[M4,C2],1,0):- M2 > 1, M3 is M1 + 2, M4 is M2 -2, we_good([M3,C1]), we_good([M4,C2]).
% Muevo 1 canibal de derecha a izquierda
move([M1,C1],[M2,C2],[M1,C3],[M2,C4],1,0):- C2 > 0, C3 is C1 + 1, C4 is C2 -1, we_good([M1,C3]), we_good([M2,C4]).
% Muevo 2 canibal de derecha a izquierda
move([M1,C1],[M2,C2],[M1,C3],[M2,C4],1,0):- C2 > 1, C3 is C1 + 2, C4 is C2 -2, we_good([M1,C3]), we_good([M2,C4]).
% Muevo 1 canibal y 1 misionero de derecha a izquierda
move([M1,C1],[M2,C2],[M3,C3],[M4,C4],1,0):- M2 > 0, C2 > 0, M3 is M1 + 1, M4 is M2 -1, C3 is C1 + 1, C4 is C2 -1,
                                            we_good([M3,C3]), we_good([M4,C4]).

notRepe(L3,L4,Canoa,Moves):- not(member([L3,L4,Canoa],Moves)).

addMove(L3,L4,Canoa,Moves,MovesNew):- concatN(Moves,[[L3,L4,Canoa]],MovesNew).

canibales([0,0],_,1,Moves):- !, write("we good boyz"), nl ,write(Moves).
canibales(L1,L2,Canoa,Moves):- move(L1,L2,L3,L4,Canoa,CanoaNew),notRepe(L3,L4,CanoaNew,Moves),
                                addMove(L3,L4,CanoaNew,Moves,MovesNew), canibales(L3,L4,CanoaNew,MovesNew).


mis:- camino( [lado1,5,5], [lado2,0,0], [[lado1,5,5]] ).

camino(Fin,Fin,Cam):- inverso(Cam,Sol), write(Sol), nl.
camino(Ini,Fin,Cam):- paso(Ini,E), novisitado(E,Cam), camino(E,Fin,[E|Cam]).

novisitado(E,Cam):- pert(E,Cam), !,fail.
novisitado(_,_).

paso( [lado1,M1,C1], [lado2,M2,C2] ):- pasan(M,C), M2 is M1-M, C2 is C1-C, safe(M2,C2).
paso( [lado2,M1,C1], [lado1,M2,C2] ):- pasan(M,C), M2 is M1+M, C2 is C1+C, safe(M2,C2).

pasan(M,C):- member( [M,C], [ [0,1], [0,2], [1,0], [1,1], [2,0] ] ).

safe(M,C):- M>=0, M=<3, C>=0, C=<3, nocomen( M, C),
            M1 is 3-M,  C1 is 3-C,  nocomen(M1,C1).

nocomen(0,_).
nocomen(M,C):- M>=C.