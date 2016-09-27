all:
	ghc -O2 Solve

clean:
	rm -f *.hi *.o Solve Create Pattern
