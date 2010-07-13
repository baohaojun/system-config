import org.antlr.runtime.*;

public class Test {
    public static void main(String[] args) throws Exception {
        // create a CharStream that reads from standard input
        ANTLRInputStream input = new ANTLRInputStream(System.in);

        // create a lexer that feeds off of input CharStream
        JavaLexer lexer = new JavaLexer(input);

        // create a buffer of tokens pulled from the lexer
        CommonTokenStream tokens = new CommonTokenStream(lexer);

        // create a parser that feeds off the tokens buffer
        JavaParser parser = new JavaParser(tokens);
        // begin parsing at rule r
        parser.compilationUnit();
    }
    class hello {}
    public static String shit = "hello world";
    private class doodx{}
    interface dooxin{}
}
