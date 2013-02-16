all: gtagsAntlrJavaParser.class JavaLexer.class JavaParser.class 
	jar cf ~/bin/Linux/ext/`uname -m`/gtagsAntlrJavaParser.jar *.class

clean:
	git clean -xfd

JavaLexer.java JavaParser.java : Java.g
	java org.antlr.Tool Java.g

gtagsAntlrJavaParser.java : JavaLexer.java

%.class : %.java
	javac $^

clean:
	git clean -fxd

test: all
	for x in *.java; do echo $$x; done | ~/bin/Linux/gtagsAntlrJavaParser

