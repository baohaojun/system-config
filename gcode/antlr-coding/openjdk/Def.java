/***
 * Excerpted from "Language Implementation Patterns",
 * published by The Pragmatic Bookshelf.
 * Copyrights apply to this code. It may not be used to create training material, 
 * courses, books, articles, and the like. Contact us if you are in doubt.
 * We make no guarantees that this code is fit for any purpose. 
 * Visit http://www.pragmaticprogrammer.com/titles/tpdsl for more book information.
***/
// $ANTLR 3.2 Sep 23, 2009 12:02:23 /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Def.g 2009-09-23 17:37:56

import org.antlr.runtime.*;
import org.antlr.runtime.tree.*;import java.util.Stack;
import java.util.List;
import java.util.ArrayList;
import java.util.Map;
import java.util.HashMap;
public class Def extends TreeFilter {
    public static final String[] tokenNames = new String[] {
        "<invalid>", "<EOR>", "<DOWN>", "<UP>", "METHOD_DECL", "ARG_DECL", "BLOCK", "MEMBERS", "VAR_DECL", "FIELD_DECL", "CALL", "ELIST", "EXPR", "ASSIGN", "EXTENDS", "ID", "INT", "LETTER", "WS", "SL_COMMENT", "'class'", "'{'", "'}'", "';'", "':'", "'public'", "'('", "')'", "','", "'float'", "'int'", "'void'", "'return'", "'+'", "'.'", "'this'", "'super'"
    };
    public static final int T__35=35;
    public static final int LETTER=17;
    public static final int T__28=28;
    public static final int T__23=23;
    public static final int T__36=36;
    public static final int T__20=20;
    public static final int EXPR=12;
    public static final int ARG_DECL=5;
    public static final int WS=18;
    public static final int T__21=21;
    public static final int FIELD_DECL=9;
    public static final int T__33=33;
    public static final int T__22=22;
    public static final int T__29=29;
    public static final int MEMBERS=7;
    public static final int BLOCK=6;
    public static final int T__30=30;
    public static final int T__31=31;
    public static final int INT=16;
    public static final int EOF=-1;
    public static final int T__27=27;
    public static final int T__32=32;
    public static final int ASSIGN=13;
    public static final int T__24=24;
    public static final int CALL=10;
    public static final int METHOD_DECL=4;
    public static final int T__26=26;
    public static final int T__25=25;
    public static final int VAR_DECL=8;
    public static final int T__34=34;
    public static final int SL_COMMENT=19;
    public static final int ELIST=11;
    public static final int EXTENDS=14;
    public static final int ID=15;

    // delegates
    // delegators


        public Def(TreeNodeStream input) {
            this(input, new RecognizerSharedState());
        }
        public Def(TreeNodeStream input, RecognizerSharedState state) {
            super(input, state);
             
        }
        

    public String[] getTokenNames() { return Def.tokenNames; }
    public String getGrammarFileName() { return "/Users/parrt/research/book/TPDSL/Book/code/symtab/class/Def.g"; }


        SymbolTable symtab;
        Scope currentScope;
        public Def(TreeNodeStream input, SymbolTable symtab) {
            this(input);
            this.symtab = symtab;
            currentScope = symtab.globals;
        }



    // $ANTLR start "topdown"
    // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Def.g:19:1: topdown : ( enterBlock | enterMethod | enterClass | varDeclaration | atoms );
    public final void topdown() throws RecognitionException {
        try {
            // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Def.g:20:5: ( enterBlock | enterMethod | enterClass | varDeclaration | atoms )
            int alt1=5;
            switch ( input.LA(1) ) {
            case BLOCK:
                {
                alt1=1;
                }
                break;
            case METHOD_DECL:
                {
                alt1=2;
                }
                break;
            case 20:
                {
                alt1=3;
                }
                break;
            case ARG_DECL:
            case VAR_DECL:
            case FIELD_DECL:
                {
                alt1=4;
                }
                break;
            case ID:
            case 35:
                {
                alt1=5;
                }
                break;
            default:
                if (state.backtracking>0) {state.failed=true; return ;}
                NoViableAltException nvae =
                    new NoViableAltException("", 1, 0, input);

                throw nvae;
            }

            switch (alt1) {
                case 1 :
                    // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Def.g:20:9: enterBlock
                    {
                    pushFollow(FOLLOW_enterBlock_in_topdown56);
                    enterBlock();

                    state._fsp--;
                    if (state.failed) return ;

                    }
                    break;
                case 2 :
                    // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Def.g:21:9: enterMethod
                    {
                    pushFollow(FOLLOW_enterMethod_in_topdown66);
                    enterMethod();

                    state._fsp--;
                    if (state.failed) return ;

                    }
                    break;
                case 3 :
                    // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Def.g:22:9: enterClass
                    {
                    pushFollow(FOLLOW_enterClass_in_topdown76);
                    enterClass();

                    state._fsp--;
                    if (state.failed) return ;

                    }
                    break;
                case 4 :
                    // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Def.g:23:9: varDeclaration
                    {
                    pushFollow(FOLLOW_varDeclaration_in_topdown86);
                    varDeclaration();

                    state._fsp--;
                    if (state.failed) return ;

                    }
                    break;
                case 5 :
                    // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Def.g:24:9: atoms
                    {
                    pushFollow(FOLLOW_atoms_in_topdown96);
                    atoms();

                    state._fsp--;
                    if (state.failed) return ;

                    }
                    break;

            }
        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {
        }
        return ;
    }
    // $ANTLR end "topdown"


    // $ANTLR start "bottomup"
    // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Def.g:27:1: bottomup : ( exitBlock | exitMethod | exitClass );
    public final void bottomup() throws RecognitionException {
        try {
            // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Def.g:28:5: ( exitBlock | exitMethod | exitClass )
            int alt2=3;
            switch ( input.LA(1) ) {
            case BLOCK:
                {
                alt2=1;
                }
                break;
            case METHOD_DECL:
                {
                alt2=2;
                }
                break;
            case 20:
                {
                alt2=3;
                }
                break;
            default:
                if (state.backtracking>0) {state.failed=true; return ;}
                NoViableAltException nvae =
                    new NoViableAltException("", 2, 0, input);

                throw nvae;
            }

            switch (alt2) {
                case 1 :
                    // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Def.g:28:9: exitBlock
                    {
                    pushFollow(FOLLOW_exitBlock_in_bottomup115);
                    exitBlock();

                    state._fsp--;
                    if (state.failed) return ;

                    }
                    break;
                case 2 :
                    // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Def.g:29:9: exitMethod
                    {
                    pushFollow(FOLLOW_exitMethod_in_bottomup125);
                    exitMethod();

                    state._fsp--;
                    if (state.failed) return ;

                    }
                    break;
                case 3 :
                    // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Def.g:30:9: exitClass
                    {
                    pushFollow(FOLLOW_exitClass_in_bottomup135);
                    exitClass();

                    state._fsp--;
                    if (state.failed) return ;

                    }
                    break;

            }
        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {
        }
        return ;
    }
    // $ANTLR end "bottomup"


    // $ANTLR start "enterBlock"
    // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Def.g:35:1: enterBlock : BLOCK ;
    public final void enterBlock() throws RecognitionException {
        try {
            // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Def.g:36:5: ( BLOCK )
            // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Def.g:36:9: BLOCK
            {
            match(input,BLOCK,FOLLOW_BLOCK_in_enterBlock156); if (state.failed) return ;
            if ( state.backtracking==1 ) {
              currentScope = new LocalScope(currentScope);
            }

            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {
        }
        return ;
    }
    // $ANTLR end "enterBlock"


    // $ANTLR start "exitBlock"
    // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Def.g:38:1: exitBlock : BLOCK ;
    public final void exitBlock() throws RecognitionException {
        try {
            // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Def.g:39:5: ( BLOCK )
            // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Def.g:39:9: BLOCK
            {
            match(input,BLOCK,FOLLOW_BLOCK_in_exitBlock177); if (state.failed) return ;
            if ( state.backtracking==1 ) {

                      System.out.println("locals: "+currentScope);
                      currentScope = currentScope.getEnclosingScope();    // pop scope
                      
            }

            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {
        }
        return ;
    }
    // $ANTLR end "exitBlock"


    // $ANTLR start "enterClass"
    // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Def.g:47:1: enterClass : ^( 'class' name= ID ( ^( EXTENDS sup= ID ) )? . ) ;
    public final void enterClass() throws RecognitionException {
        CymbolAST name=null;
        CymbolAST sup=null;

        try {
            // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Def.g:48:5: ( ^( 'class' name= ID ( ^( EXTENDS sup= ID ) )? . ) )
            // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Def.g:48:9: ^( 'class' name= ID ( ^( EXTENDS sup= ID ) )? . )
            {
            match(input,20,FOLLOW_20_in_enterClass208); if (state.failed) return ;

            match(input, Token.DOWN, null); if (state.failed) return ;
            name=(CymbolAST)match(input,ID,FOLLOW_ID_in_enterClass212); if (state.failed) return ;
            // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Def.g:48:27: ( ^( EXTENDS sup= ID ) )?
            int alt3=2;
            int LA3_0 = input.LA(1);

            if ( (LA3_0==EXTENDS) ) {
                int LA3_1 = input.LA(2);

                if ( (LA3_1==DOWN) ) {
                    int LA3_3 = input.LA(3);

                    if ( (LA3_3==ID) ) {
                        int LA3_4 = input.LA(4);

                        if ( (LA3_4==UP) ) {
                            int LA3_5 = input.LA(5);

                            if ( ((LA3_5>=METHOD_DECL && LA3_5<=36)) ) {
                                alt3=1;
                            }
                        }
                    }
                }
            }
            switch (alt3) {
                case 1 :
                    // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Def.g:48:28: ^( EXTENDS sup= ID )
                    {
                    match(input,EXTENDS,FOLLOW_EXTENDS_in_enterClass216); if (state.failed) return ;

                    match(input, Token.DOWN, null); if (state.failed) return ;
                    sup=(CymbolAST)match(input,ID,FOLLOW_ID_in_enterClass220); if (state.failed) return ;

                    match(input, Token.UP, null); if (state.failed) return ;

                    }
                    break;

            }

            matchAny(input); if (state.failed) return ;

            match(input, Token.UP, null); if (state.failed) return ;
            if ( state.backtracking==1 ) {
               // def class but leave superclass blank until ref phase
                      System.out.println("line "+name.getLine()+
                                         ": def class "+(name!=null?name.getText():null));
                      // record scope in AST for next pass
                      if ( sup!=null ) sup.scope = currentScope; 
                      ClassSymbol cs = new ClassSymbol((name!=null?name.getText():null),currentScope,null);
                      cs.def = name;           // point from symbol table into AST
                      name.symbol = cs;        // point from AST into symbol table
                      currentScope.define(cs);  // def class in current scope
                      currentScope = cs;        // set current scope to class scope
                      
            }

            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {
        }
        return ;
    }
    // $ANTLR end "enterClass"


    // $ANTLR start "exitClass"
    // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Def.g:63:1: exitClass : 'class' ;
    public final void exitClass() throws RecognitionException {
        try {
            // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Def.g:64:5: ( 'class' )
            // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Def.g:64:9: 'class'
            {
            match(input,20,FOLLOW_20_in_exitClass256); if (state.failed) return ;
            if ( state.backtracking==1 ) {

                      System.out.println("members: "+currentScope);
                      currentScope = currentScope.getEnclosingScope();    // pop scope
                      
            }

            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {
        }
        return ;
    }
    // $ANTLR end "exitClass"


    // $ANTLR start "enterMethod"
    // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Def.g:71:1: enterMethod : ^( METHOD_DECL type= . ID ( . )* ) ;
    public final void enterMethod() throws RecognitionException {
        CymbolAST ID1=null;
        CymbolAST type=null;

        try {
            // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Def.g:72:5: ( ^( METHOD_DECL type= . ID ( . )* ) )
            // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Def.g:72:9: ^( METHOD_DECL type= . ID ( . )* )
            {
            match(input,METHOD_DECL,FOLLOW_METHOD_DECL_in_enterMethod286); if (state.failed) return ;

            match(input, Token.DOWN, null); if (state.failed) return ;
            type=(CymbolAST)input.LT(1);
            matchAny(input); if (state.failed) return ;
            ID1=(CymbolAST)match(input,ID,FOLLOW_ID_in_enterMethod292); if (state.failed) return ;
            // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Def.g:72:33: ( . )*
            loop4:
            do {
                int alt4=2;
                int LA4_0 = input.LA(1);

                if ( ((LA4_0>=METHOD_DECL && LA4_0<=36)) ) {
                    alt4=1;
                }
                else if ( (LA4_0==UP) ) {
                    alt4=2;
                }


                switch (alt4) {
            	case 1 :
            	    // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Def.g:72:33: .
            	    {
            	    matchAny(input); if (state.failed) return ;

            	    }
            	    break;

            	default :
            	    break loop4;
                }
            } while (true);


            match(input, Token.UP, null); if (state.failed) return ;
            if ( state.backtracking==1 ) {

                      System.out.println("line "+ID1.getLine()+": def method "+(ID1!=null?ID1.getText():null));
                      type.scope = currentScope;
                      MethodSymbol ms = new MethodSymbol((ID1!=null?ID1.getText():null),null,currentScope);
                      ms.def = ID1;            // track AST location of def's ID
                      ID1.symbol = ms;         // track in AST
                      currentScope.define(ms); // def method in globals
                      currentScope = ms;       // set current scope to method scope
                      
            }

            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {
        }
        return ;
    }
    // $ANTLR end "enterMethod"


    // $ANTLR start "exitMethod"
    // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Def.g:83:1: exitMethod : METHOD_DECL ;
    public final void exitMethod() throws RecognitionException {
        try {
            // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Def.g:84:5: ( METHOD_DECL )
            // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Def.g:84:9: METHOD_DECL
            {
            match(input,METHOD_DECL,FOLLOW_METHOD_DECL_in_exitMethod325); if (state.failed) return ;
            if ( state.backtracking==1 ) {

                      System.out.println("args: "+currentScope);
                      currentScope = currentScope.getEnclosingScope();    // pop arg scope
                      
            }

            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {
        }
        return ;
    }
    // $ANTLR end "exitMethod"


    // $ANTLR start "atoms"
    // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Def.g:92:1: atoms : {...}? ( 'this' | ID ) ;
    public final void atoms() throws RecognitionException {
        CymbolAST t = (CymbolAST)input.LT(1);
        try {
            // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Def.g:95:5: ({...}? ( 'this' | ID ) )
            // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Def.g:95:8: {...}? ( 'this' | ID )
            {
            if ( !((t.hasAncestor(EXPR)||t.hasAncestor(ASSIGN))) ) {
                if (state.backtracking>0) {state.failed=true; return ;}
                throw new FailedPredicateException(input, "atoms", "t.hasAncestor(EXPR)||t.hasAncestor(ASSIGN)");
            }
            if ( input.LA(1)==ID||input.LA(1)==35 ) {
                input.consume();
                state.errorRecovery=false;state.failed=false;
            }
            else {
                if (state.backtracking>0) {state.failed=true; return ;}
                MismatchedSetException mse = new MismatchedSetException(null,input);
                throw mse;
            }

            if ( state.backtracking==1 ) {
              t.scope = currentScope;
            }

            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {
        }
        return ;
    }
    // $ANTLR end "atoms"


    // $ANTLR start "varDeclaration"
    // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Def.g:101:1: varDeclaration : ^( ( FIELD_DECL | VAR_DECL | ARG_DECL ) type= . ID ( . )? ) ;
    public final void varDeclaration() throws RecognitionException {
        CymbolAST ID2=null;
        CymbolAST type=null;

        try {
            // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Def.g:102:5: ( ^( ( FIELD_DECL | VAR_DECL | ARG_DECL ) type= . ID ( . )? ) )
            // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Def.g:102:9: ^( ( FIELD_DECL | VAR_DECL | ARG_DECL ) type= . ID ( . )? )
            {
            if ( input.LA(1)==ARG_DECL||(input.LA(1)>=VAR_DECL && input.LA(1)<=FIELD_DECL) ) {
                input.consume();
                state.errorRecovery=false;state.failed=false;
            }
            else {
                if (state.backtracking>0) {state.failed=true; return ;}
                MismatchedSetException mse = new MismatchedSetException(null,input);
                throw mse;
            }


            match(input, Token.DOWN, null); if (state.failed) return ;
            type=(CymbolAST)input.LT(1);
            matchAny(input); if (state.failed) return ;
            ID2=(CymbolAST)match(input,ID,FOLLOW_ID_in_varDeclaration411); if (state.failed) return ;
            // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Def.g:102:52: ( . )?
            int alt5=2;
            int LA5_0 = input.LA(1);

            if ( ((LA5_0>=METHOD_DECL && LA5_0<=36)) ) {
                alt5=1;
            }
            switch (alt5) {
                case 1 :
                    // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Def.g:102:52: .
                    {
                    matchAny(input); if (state.failed) return ;

                    }
                    break;

            }


            match(input, Token.UP, null); if (state.failed) return ;
            if ( state.backtracking==1 ) {

                      System.out.println("line "+ID2.getLine()+": def "+(ID2!=null?ID2.getText():null));
                      type.scope = currentScope;
                      VariableSymbol vs = new VariableSymbol((ID2!=null?ID2.getText():null),null);
                      vs.def = ID2;            // track AST location of def's ID
                      ID2.symbol = vs;         // track in AST
                      currentScope.define(vs);
                      
            }

            }

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {
        }
        return ;
    }
    // $ANTLR end "varDeclaration"

    // Delegated rules


 

    public static final BitSet FOLLOW_enterBlock_in_topdown56 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_enterMethod_in_topdown66 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_enterClass_in_topdown76 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_varDeclaration_in_topdown86 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_atoms_in_topdown96 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_exitBlock_in_bottomup115 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_exitMethod_in_bottomup125 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_exitClass_in_bottomup135 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_BLOCK_in_enterBlock156 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_BLOCK_in_exitBlock177 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_20_in_enterClass208 = new BitSet(new long[]{0x0000000000000004L});
    public static final BitSet FOLLOW_ID_in_enterClass212 = new BitSet(new long[]{0x0000001FFFFFFFF0L});
    public static final BitSet FOLLOW_EXTENDS_in_enterClass216 = new BitSet(new long[]{0x0000000000000004L});
    public static final BitSet FOLLOW_ID_in_enterClass220 = new BitSet(new long[]{0x0000000000000008L});
    public static final BitSet FOLLOW_20_in_exitClass256 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_METHOD_DECL_in_enterMethod286 = new BitSet(new long[]{0x0000000000000004L});
    public static final BitSet FOLLOW_ID_in_enterMethod292 = new BitSet(new long[]{0x0000001FFFFFFFF8L});
    public static final BitSet FOLLOW_METHOD_DECL_in_exitMethod325 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_set_in_atoms363 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_set_in_varDeclaration399 = new BitSet(new long[]{0x0000000000000004L});
    public static final BitSet FOLLOW_ID_in_varDeclaration411 = new BitSet(new long[]{0x0000001FFFFFFFF8L});

}