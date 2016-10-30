ALL = Solve Create Pattern Color Uncolor Min

PACKAGE = net/tommay/spudoku

Solve: build/$(PACKAGE)/Solve.class

all: $(ALL)

$(ALL): FORCE
FORCE:

build/$(PACKAGE)/%.class: $(PACKAGE)/%.fr
	[ -d build ] || mkdir build
	java -Xss1m -jar fregec.jar -d build -make $<

clean:
	rm -f *.hi *.o $(ALL)
