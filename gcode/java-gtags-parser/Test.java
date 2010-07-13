import org.antlr.runtime.*;
import java.io.*;

public class Test {
    public static void main(String[] args) throws Exception {
        BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
        String fileName;
        while ((fileName = in.readLine()) != null) {
            fileName = fileName.trim();
            
        // create a CharStream that reads from standard input
            ANTLRInputStream input = new ANTLRInputStream(new FileInputStream(fileName));

        // create a lexer that feeds off of input CharStream
            JavaLexer lexer = new JavaLexer(input);

        // create a buffer of tokens pulled from the lexer
            CommonTokenStream tokens = new CommonTokenStream(lexer);

        // create a parser that feeds off the tokens buffer
            JavaParser parser = new JavaParser(tokens, fileName);
        // begin parsing at rule r
            parser.compilationUnit();
            System.out.println("###terminator###");
        }
    }
    class hello {}
    public static String shit = "hello world";
    private class doodx{}
    interface dooxin{}
}
