all: OrgModeLexer.java
	javac *.java
	java Test < test.org

OrgModeLexer.java OrgModeParser.java : OrgMode.gen
	touch "$@"

%.gen : %.g
	touch $@
	antlr3 $< 2>&1|perl -npe 's/^(warning|error)\(\d+\): //'

