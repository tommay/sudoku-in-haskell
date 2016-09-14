all:
	ghc -O Sudoku
	ghc -O Create

clean:
	rm -f *.hi *.o Create Sudoku
