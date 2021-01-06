:- use_module(library(clpfd)).

ejemplo(0,   26, [1,2,5,10] ).  % Solution: [1,0,1,2]
ejemplo(1,  361, [1,2,5,13,17,35,157]).

main:-
    ejemplo(1,Amount,Coins),
    nl, write('Paying amount '), write(Amount), write(' using the minimal number of coins of values '), write(Coins), nl,nl,
    length(Coins,N),
    length(Vars,N), % get list of N prolog vars
    Vars ins 0..Amount,
    sumVars(Vars,Coins,Sum),
    Sum #= Amount,
    numberCoins(Vars,NCoins),
    labeling([min(NCoins)],Vars),
    nl, write(Vars), nl,nl, halt.


numberCoins([X],X):- !.

numberCoins([X|L],X + R):-
  numberCoins(L,R).

sumVars([X],[F],X*F):- !.

sumVars([X|Left],[C|Left2],X * C + Rest):-
    sumVars(Left,Left2,Rest).
