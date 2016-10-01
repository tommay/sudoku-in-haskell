ALL = Solve Create Pattern Color Uncolor

all: $(ALL)

$(ALL): FORCE
FORCE:

%: %.hs
	ghc -O2 $<

clean:
	rm -f *.hi *.o $(ALL)
