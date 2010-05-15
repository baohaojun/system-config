all:
	java org.antlr.Tool Java.g
	javac *.java
	jar cfv ~/bin/linux/ext/uncomment-java.jar *.class

clean:
	svn-clean
