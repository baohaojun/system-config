JAVA_SRCS = BaseScope.java BuiltInTypeSymbol.java ClassSymbol.java Def.java GlobalScope.java JnuAST.java JnuErrorNode.java JnuLexer.java JnuParser.java LocalScope.java MethodSymbol.java Ref.java ScopedSymbol.java Scope.java Symbol.java SymbolTable.java Test.java Type.java VariableSymbol.java

all: $(JAVA_SRCS)
	javac *.java

Def.java : Def.gen
Ref.java : Ref.gen

Def.gen : Jnu.gen
Ref.gen : Jnu.gen


JnuAST.java JnuErrorNode.java JnuLexer.java JnuParser.java : Jnu.gen
	touch "$@"

%.gen : %.g
	touch $@
	java -cp ~/today/antlrworks-1.4.3.jar org.antlr.Tool $< 2>&1|perl -npe 's/^(warning|error)\(\d+\): //'

