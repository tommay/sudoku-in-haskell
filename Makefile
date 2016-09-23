all:
	ghc -O2 Solve
	ghc -O2 Create
	ghc -O2 Pattern

clean:
	rm -f *.hi *.o Colve Create
