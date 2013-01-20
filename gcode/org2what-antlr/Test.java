/***
 * Excerpted from "The Definitive ANTLR Reference",
 * published by The Pragmatic Bookshelf.
 * Copyrights apply to this code. It may not be used to create training material, 
 * courses, books, articles, and the like. Contact us if you are in doubt.
 * We make no guarantees that this code is fit for any purpose. 
 * Visit http://www.pragmaticprogrammer.com/titles/tpantlr for more book information.
***/
import org.antlr.runtime.*;
import org.antlr.stringtemplate.*;
import java.io.*;

public class Test {
    public static void main(String[] args) throws Exception {

        ANTLRInputStream input = new ANTLRInputStream(System.in);
        OrgModeLexer lexer = new OrgModeLexer(input);
        TokenRewriteStream tokens = new TokenRewriteStream(lexer);
        OrgModeParser parser = new OrgModeParser(tokens);
        parser.orgFile();
    }
}
