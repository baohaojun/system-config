/***
 * Excerpted from "Language Implementation Patterns",
 * published by The Pragmatic Bookshelf.
 * Copyrights apply to this code. It may not be used to create training material, 
 * courses, books, articles, and the like. Contact us if you are in doubt.
 * We make no guarantees that this code is fit for any purpose. 
 * Visit http://www.pragmaticprogrammer.com/titles/tpdsl for more book information.
***/
import org.antlr.runtime.*;
import org.antlr.runtime.tree.*;

public class Test {
    /** An adaptor that tells ANTLR to build CymbolAST nodes */
    public static TreeAdaptor cymbalAdaptor = new CommonTreeAdaptor() {
        public Object create(Token token) {
            return new CymbolAST(token);
        }
        public Object dupNode(Object t) {
            if ( t==null ) {
                return null;
            }
            return create(((CymbolAST)t).token);
        }
        public Object errorNode(TokenStream input, Token start, Token stop,
                                RecognitionException e)
        {
            CymbolErrorNode t = new CymbolErrorNode(input, start, stop, e);
            //System.out.println("returning error node '"+t+"' @index="+input.index());
            return t;
        }
    };

    public static void main(String[] args) throws Exception {
        CharStream input = null;
        if ( args.length>0 ) input = new ANTLRFileStream(args[0]);
        else input = new ANTLRInputStream(System.in);
        // Create lexer/parser to build trees from stdin
        CymbolLexer lex = new CymbolLexer(input);
        CommonTokenStream tokens = new CommonTokenStream(lex);
        CymbolParser p = new CymbolParser(tokens);
        p.setTreeAdaptor(cymbalAdaptor);
        RuleReturnScope r = p.compilationUnit();   // launch parser by calling start rule
        CommonTree t = (CommonTree)r.getTree();    // get tree result
        //System.out.println("tree: "+t.toStringTree());
        //DOTTreeGenerator dot = new DOTTreeGenerator();
        //System.out.println(dot.toDOT(t));

        CommonTreeNodeStream nodes = new CommonTreeNodeStream(cymbalAdaptor, t);
        nodes.setTokenStream(tokens);
        SymbolTable symtab = new SymbolTable(); // init symbol table
        Def def = new Def(nodes, symtab);       // create Def phase
        def.downup(t);                          // Do pass 1
        System.out.println("globals: "+symtab.globals);
        nodes.reset(); // rewind AST node stream to root
        Ref ref = new Ref(nodes);               // create Ref phase
        ref.downup(t);                          // Do pass 2
    }
}
