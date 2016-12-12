ALL = Solve Create Pattern # Color Uncolor Min

PACKAGE = net/tommay/spudoku

all: $(ALL)

$(ALL): FORCE
FORCE:

Solve: build/$(PACKAGE)/Solve.class
Create: build/$(PACKAGE)/Create.class
Pattern: build/$(PACKAGE)/Pattern.class

build/$(PACKAGE)/%.class: $(PACKAGE)/%.fr
	[ -d build ] || mkdir build
	java -Xss1m -jar fregec.jar -O -d build -make $<

clean:
	rm -fr build
