#file = miSudoku
#file = roads
#file = olympiad
file = tsp

$(file): $(file).pl
	swipl -q -O -g main --stand_alone=true -o $(file) -c $(file).pl


