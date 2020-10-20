concatN([],L,L).
concatN([X|L1],L2,[X|L3]) :- concatN(L1,L2,L3).

pert(X,[X|_]).
pert(X,[_|L]):- pert(X,L).

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

inverse([X],[X]).
inverse(L,R):- concatN(L1,[X],L), inverse(L1,R1), concatN([X],R1,R).

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
ordenada tiene coste lineal, por eso el coste es O((n+1)!) 
