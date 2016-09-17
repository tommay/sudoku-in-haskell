all:
	ghc -O2 Sudoku

clean:
	rm -f *.hi *.o Sudoku
