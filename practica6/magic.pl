:- use_module(library(clpfd)).

% Complete the following program p(N) that writes a (kind of) magic
% square: an NxN matrix with ALL the numbers 1..N^2, such that the
% sum of every row and every column is equal.
% More precisely, this sum is (N + N^3) / 2.
% Note: don't worry if your program is (too) slow when N >= 6.

%% Example:  (this solution is not unique):
%%
%%    1   2  13  24  25
%%    3   8  18  17  19
%%   16  14  15   9  11
%%   22  20   7  10   6
%%   23  21  12   5   4

main:- p(6), nl, halt.

p(N):-
    NSquare is N*N,
    length( Vars, NSquare ),
    Vars ins 1..NSquare,
    squareByRows(N,Vars,SquareByRows),
    transpose( SquareByRows, SquareByCols ),  % transpose already exists: no need to implement it
    Sum is (N + N*N*N) // 2,
    all_distinct(Vars),
    constraintsSum(Sum, SquareByRows),
    constraintsSum(Sum, SquareByCols),
    labeling([ff],Vars),
    writeSquare(SquareByRows),nl,!.

constraintsSum(_,[]):- !.
constraintsSum(Sum, [Row|Rows]):-
  sum_list(Row,S),
  S #= Sum,
  constraintsSum(Sum,Rows).

sum_list([],0):- !.
sum_list([E|Elems],E+S):-
  sum_list(Elems,S).


squareByRows(_,[],[]):-!.
squareByRows(N,Vars,[Row|SquareByRows]):- append(Row,Vars1,Vars), length(Row,N), squareByRows(N,Vars1,SquareByRows),!.

writeSquare(Square):- member(Row,Square), nl, member(N,Row), write4(N), fail.
writeSquare(_).

write4(N):- N<10,   write('   '), write(N),!.
write4(N):- N<100,  write('  ' ), write(N),!.
write4(N):-         write(' '  ), write(N),!.
