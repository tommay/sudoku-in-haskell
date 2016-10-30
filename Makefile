ALL = Solve Create Pattern Color Uncolor Min

PACKAGE = net/tommay/spudoku
BUILD = build/$(PACKAGE)

Solve: $(BUILD)/Solve.class

all: $(ALL)

$(ALL): FORCE
FORCE:

$(BUILD)/%.class: $(PACKAGE)/%.fr
	[ -d $(BUILD) ] || mkdir -p $(BUILD)
	java -Xss1m -jar fregec.jar -d $(BUILD) -make $<

clean:
	rm -f *.hi *.o $(ALL)
