all: gtagsAntlrJavaParser.class JavaLexer.class JavaParser.class
	jar cf ~/bin/linux/ext/`uname -m`/gtagsAntlrJavaParser.jar *.class

JavaLexer.java JavaParser.java : Java.g
	java org.antlr.Tool Java.g

gtagsAntlrJavaParser.java : JavaLexer.java

%.class : %.java
	javac $^

clean:
	git clean -fxd

test: all
	for x in *.java; do echo $$x; done | ~/bin/linux/gtagsAntlrJavaParser
