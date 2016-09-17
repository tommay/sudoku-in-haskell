all:
	ghc -O2 Sudoku
	ghc -O2 Create

clean:
	rm -f *.hi *.o Create Sudoku
