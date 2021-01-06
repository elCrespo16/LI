:- use_module(library(clpfd)).

%% A (6-sided) "letter dice" has on each side a different letter.
%% Find four of them, with the 24 letters abcdefghijklmnoprstuvwxy such
%% that you can make all the following words: bake, onyx, echo, oval,
%% gird, smug, jump, torn, luck, viny, lush, wrap.

%Some helpful predicates:

word( [b,a,k,e] ).
word( [o,n,y,x] ).
word( [e,c,h,o] ).
word( [o,v,a,l] ).
word( [g,i,r,d] ).
word( [s,m,u,g] ).
word( [j,u,m,p] ).
word( [t,o,r,n] ).
word( [l,u,c,k] ).
word( [v,i,n,y] ).
word( [l,u,s,h] ).
word( [w,r,a,p] ).

num(X,N):- nth1( N, [a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,r,s,t,u,v,w,x,y], X ).

constraints(D1,D2,D3,D4,[[L1,L2,L3,L4]|Palabras]):-
  member(Cara1,D1),
  member(Cara2,D2),
  member(Cara3,D3),
  member(Cara4,D4),
  num(L1,Letra1),
  num(L2,Letra2),
  num(L3,Letra3),
  num(L4,Letra4),
  Cara1 #= Letra1 #\/ Cara1 #= Letra2 #\/ Cara1 #= Letra3 #\/ Cara1 #= Letra4,
  Cara2 #= Letra1 #\/ Cara2 #= Letra2 #\/ Cara2 #= Letra3 #\/ Cara2 #= Letra4,
  Cara3 #= Letra1 #\/ Cara3 #= Letra2 #\/ Cara3 #= Letra3 #\/ Cara3 #= Letra4,
  Cara4 #= Letra1 #\/ Cara4 #= Letra2 #\/ Cara4 #= Letra3 #\/ Cara4 #= Letra4,
  constraints(D1,D2,D3,D4,Palabras).

constraints(_,_,_,_,[]).


main:-
    length(D1,6),
    length(D2,6),
    length(D3,6),
    length(D4,6),

    D1 ins 1..24,
    D2 ins 1..24,
    D3 ins 1..24,
    D4 ins 1..24,

    append(D1,D2,A1),
    append(A1,D3,A2),
    append(A2,D4,Todos),

    all_distinct(Todos),

    findall(Palabra,word(Palabra),Palabras),
    constraints(D1,D2,D3,D4,Palabras),

    labeling([],Todos),
    writeN(D1),
    writeN(D2),
    writeN(D3),
    writeN(D4),
    halt.

writeN(D):- findall(X,(member(N,D),num(X,N)),L), write(L), nl, !.
