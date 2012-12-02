all:
	java -cp ~/today/antlrworks-1.4.3.jar org.antlr.Tool Jnu.g 2>&1|perl -npe 's/^(warning|error)\(\d+\): //'
	java -cp ~/today/antlrworks-1.4.3.jar org.antlr.Tool Def.g 2>&1|perl -npe 's/^(warning|error)\(\d+\): //'
	java -cp ~/today/antlrworks-1.4.3.jar org.antlr.Tool Ref.g 2>&1|perl -npe 's/^(warning|error)\(\d+\): //'
	javac *.java
