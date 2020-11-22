#file = minColoring
#file = basket
file = factory

$(file): $(file).pl
	swipl -O -q -g main --stand_alone=true -o $(file) -c $(file).pl
