all:
	java org.antlr.Tool *.g
	javac *.java
	jar cfv ~/bin/linux/ext/BeagleTokens.jar *.class

clean:
	svn-clean
