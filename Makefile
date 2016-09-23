all:
	ghc -O2 Solve
	ghc -O2 Create

clean:
	rm -f *.hi *.o Colve Create
