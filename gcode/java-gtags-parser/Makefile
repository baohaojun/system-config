all: gtagsAntlrJavaParser.class JavaLexer.class JavaParser.class private_ok/JavaLexer.class private_ok/JavaParser.class private_ok/gtagsAntlrJavaParser.class
	jar cf ~/bin/linux/ext/`uname -m`/gtagsAntlrJavaParser.jar *.class
	cd private_ok && jar cf ~/bin/linux/ext/`uname -m`/gtagsAntlrJavaParserPrivateOk.jar *.class

clean:
	git clean -xfd

JavaLexer.java JavaParser.java : Java.g
	java org.antlr.Tool Java.g

private_ok/JavaLexer.java private_ok/JavaParser.java : Java.g
	mkdir -p private_ok
	cd private_ok && perl -npe 's/is_private = true/is_private = false/g' ../Java.g >./Java.g && java org.antlr.Tool Java.g && javac *.java

gtagsAntlrJavaParser.java : JavaLexer.java

private_ok/gtagsAntlrJavaParser.java : gtagsAntlrJavaParser.java
	cp $^ $@

%.class : %.java
	javac $^

private_ok/%.class : private_ok/%.java
	cd private_ok/ && javac `basename $^`


clean:
	git clean -fxd

test: all
	for x in *.java; do echo $$x; done | ~/bin/linux/gtagsAntlrJavaParser

testPrivateOk: all
	for x in *.java; do echo $$x; done | ~/bin/linux/gtagsAntlrJavaParserPrivateOk
