import org.antlr.runtime.*;
import org.antlr.stringtemplate.*;
import java.io.*;

public class Testx {
    public static void main(String[] args) throws Exception {
	
        ANTLRInputStream input = new ANTLRInputStream(System.in);
        Test1Lexer lexer = new Test1Lexer(input);
        // rewrite=true only works with TokenRewriteStream
        // not CommonTokenStream!
        TokenRewriteStream tokens = new TokenRewriteStream(lexer);
        Test1Parser parser = new Test1Parser(tokens);
        parser.head();
        System.out.println(tokens.toString()); // emit rewritten source
	
    }
}

