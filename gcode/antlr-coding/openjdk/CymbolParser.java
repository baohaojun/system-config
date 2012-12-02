/***
 * Excerpted from "Language Implementation Patterns",
 * published by The Pragmatic Bookshelf.
 * Copyrights apply to this code. It may not be used to create training material, 
 * courses, books, articles, and the like. Contact us if you are in doubt.
 * We make no guarantees that this code is fit for any purpose. 
 * Visit http://www.pragmaticprogrammer.com/titles/tpdsl for more book information.
***/
// $ANTLR 3.2 Sep 23, 2009 12:02:23 /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Cymbol.g 2009-09-23 17:37:56

import org.antlr.runtime.*;
import java.util.Stack;
import java.util.List;
import java.util.ArrayList;
import java.util.Map;
import java.util.HashMap;

import org.antlr.runtime.tree.*;

public class CymbolParser extends Parser {
    public static final String[] tokenNames = new String[] {
        "<invalid>", "<EOR>", "<DOWN>", "<UP>", "METHOD_DECL", "ARG_DECL", "BLOCK", "MEMBERS", "VAR_DECL", "FIELD_DECL", "CALL", "ELIST", "EXPR", "ASSIGN", "EXTENDS", "ID", "INT", "LETTER", "WS", "SL_COMMENT", "'class'", "'{'", "'}'", "';'", "':'", "'public'", "'('", "')'", "','", "'float'", "'int'", "'void'", "'return'", "'+'", "'.'", "'this'", "'super'"
    };
    public static final int LETTER=17;
    public static final int T__35=35;
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
    public static final int INT=16;
    public static final int T__31=31;
    public static final int EOF=-1;
    public static final int T__27=27;
    public static final int ASSIGN=13;
    public static final int T__32=32;
    public static final int CALL=10;
    public static final int T__24=24;
    public static final int METHOD_DECL=4;
    public static final int T__26=26;
    public static final int T__25=25;
    public static final int VAR_DECL=8;
    public static final int T__34=34;
    public static final int SL_COMMENT=19;
    public static final int EXTENDS=14;
    public static final int ELIST=11;
    public static final int ID=15;

    // delegates
    // delegators


        public CymbolParser(TokenStream input) {
            this(input, new RecognizerSharedState());
        }
        public CymbolParser(TokenStream input, RecognizerSharedState state) {
            super(input, state);
             
        }
        
    protected TreeAdaptor adaptor = new CommonTreeAdaptor();

    public void setTreeAdaptor(TreeAdaptor adaptor) {
        this.adaptor = adaptor;
    }
    public TreeAdaptor getTreeAdaptor() {
        return adaptor;
    }

    public String[] getTokenNames() { return CymbolParser.tokenNames; }
    public String getGrammarFileName() { return "/Users/parrt/research/book/TPDSL/Book/code/symtab/class/Cymbol.g"; }


    public static class compilationUnit_return extends ParserRuleReturnScope {
        CymbolAST tree;
        public Object getTree() { return tree; }
    };

    // $ANTLR start "compilationUnit"
    // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Cymbol.g:21:1: compilationUnit : ( classDefinition | varDeclaration | methodDeclaration )+ EOF ;
    public final CymbolParser.compilationUnit_return compilationUnit() throws RecognitionException {
        CymbolParser.compilationUnit_return retval = new CymbolParser.compilationUnit_return();
        retval.start = input.LT(1);

        CymbolAST root_0 = null;

        Token EOF4=null;
        CymbolParser.classDefinition_return classDefinition1 = null;

        CymbolParser.varDeclaration_return varDeclaration2 = null;

        CymbolParser.methodDeclaration_return methodDeclaration3 = null;


        CymbolAST EOF4_tree=null;

        try {
            // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Cymbol.g:22:5: ( ( classDefinition | varDeclaration | methodDeclaration )+ EOF )
            // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Cymbol.g:22:9: ( classDefinition | varDeclaration | methodDeclaration )+ EOF
            {
            root_0 = (CymbolAST)adaptor.nil();

            // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Cymbol.g:22:9: ( classDefinition | varDeclaration | methodDeclaration )+
            int cnt1=0;
            loop1:
            do {
                int alt1=4;
                int LA1_0 = input.LA(1);

                if ( (LA1_0==20) ) {
                    alt1=1;
                }
                else if ( (LA1_0==ID||(LA1_0>=29 && LA1_0<=31)) ) {
                    int LA1_3 = input.LA(2);

                    if ( (LA1_3==ID) ) {
                        int LA1_4 = input.LA(3);

                        if ( (LA1_4==26) ) {
                            alt1=3;
                        }
                        else if ( (LA1_4==ASSIGN||LA1_4==23) ) {
                            alt1=2;
                        }


                    }


                }


                switch (alt1) {
            	case 1 :
            	    // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Cymbol.g:22:11: classDefinition
            	    {
            	    pushFollow(FOLLOW_classDefinition_in_compilationUnit138);
            	    classDefinition1=classDefinition();

            	    state._fsp--;
            	    if (state.failed) return retval;
            	    if ( state.backtracking==0 ) adaptor.addChild(root_0, classDefinition1.getTree());

            	    }
            	    break;
            	case 2 :
            	    // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Cymbol.g:22:29: varDeclaration
            	    {
            	    pushFollow(FOLLOW_varDeclaration_in_compilationUnit142);
            	    varDeclaration2=varDeclaration();

            	    state._fsp--;
            	    if (state.failed) return retval;
            	    if ( state.backtracking==0 ) adaptor.addChild(root_0, varDeclaration2.getTree());

            	    }
            	    break;
            	case 3 :
            	    // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Cymbol.g:22:46: methodDeclaration
            	    {
            	    pushFollow(FOLLOW_methodDeclaration_in_compilationUnit146);
            	    methodDeclaration3=methodDeclaration();

            	    state._fsp--;
            	    if (state.failed) return retval;
            	    if ( state.backtracking==0 ) adaptor.addChild(root_0, methodDeclaration3.getTree());

            	    }
            	    break;

            	default :
            	    if ( cnt1 >= 1 ) break loop1;
            	    if (state.backtracking>0) {state.failed=true; return retval;}
                        EarlyExitException eee =
                            new EarlyExitException(1, input);
                        throw eee;
                }
                cnt1++;
            } while (true);

            EOF4=(Token)match(input,EOF,FOLLOW_EOF_in_compilationUnit151); if (state.failed) return retval;
            if ( state.backtracking==0 ) {
            EOF4_tree = (CymbolAST)adaptor.create(EOF4);
            adaptor.addChild(root_0, EOF4_tree);
            }

            }

            retval.stop = input.LT(-1);

            if ( state.backtracking==0 ) {

            retval.tree = (CymbolAST)adaptor.rulePostProcessing(root_0);
            adaptor.setTokenBoundaries(retval.tree, retval.start, retval.stop);
            }
        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
    	retval.tree = (CymbolAST)adaptor.errorNode(input, retval.start, input.LT(-1), re);

        }
        finally {
        }
        return retval;
    }
    // $ANTLR end "compilationUnit"

    public static class classDefinition_return extends ParserRuleReturnScope {
        CymbolAST tree;
        public Object getTree() { return tree; }
    };

    // $ANTLR start "classDefinition"
    // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Cymbol.g:26:1: classDefinition : 'class' ID ( superClass )? '{' ( classMember )+ '}' ';' -> ^( 'class' ID ( superClass )? ^( MEMBERS ( classMember )+ ) ) ;
    public final CymbolParser.classDefinition_return classDefinition() throws RecognitionException {
        CymbolParser.classDefinition_return retval = new CymbolParser.classDefinition_return();
        retval.start = input.LT(1);

        CymbolAST root_0 = null;

        Token string_literal5=null;
        Token ID6=null;
        Token char_literal8=null;
        Token char_literal10=null;
        Token char_literal11=null;
        CymbolParser.superClass_return superClass7 = null;

        CymbolParser.classMember_return classMember9 = null;


        CymbolAST string_literal5_tree=null;
        CymbolAST ID6_tree=null;
        CymbolAST char_literal8_tree=null;
        CymbolAST char_literal10_tree=null;
        CymbolAST char_literal11_tree=null;
        RewriteRuleTokenStream stream_21=new RewriteRuleTokenStream(adaptor,"token 21");
        RewriteRuleTokenStream stream_22=new RewriteRuleTokenStream(adaptor,"token 22");
        RewriteRuleTokenStream stream_20=new RewriteRuleTokenStream(adaptor,"token 20");
        RewriteRuleTokenStream stream_23=new RewriteRuleTokenStream(adaptor,"token 23");
        RewriteRuleTokenStream stream_ID=new RewriteRuleTokenStream(adaptor,"token ID");
        RewriteRuleSubtreeStream stream_classMember=new RewriteRuleSubtreeStream(adaptor,"rule classMember");
        RewriteRuleSubtreeStream stream_superClass=new RewriteRuleSubtreeStream(adaptor,"rule superClass");
        try {
            // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Cymbol.g:27:5: ( 'class' ID ( superClass )? '{' ( classMember )+ '}' ';' -> ^( 'class' ID ( superClass )? ^( MEMBERS ( classMember )+ ) ) )
            // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Cymbol.g:27:9: 'class' ID ( superClass )? '{' ( classMember )+ '}' ';'
            {
            string_literal5=(Token)match(input,20,FOLLOW_20_in_classDefinition171); if (state.failed) return retval; 
            if ( state.backtracking==0 ) stream_20.add(string_literal5);

            ID6=(Token)match(input,ID,FOLLOW_ID_in_classDefinition173); if (state.failed) return retval; 
            if ( state.backtracking==0 ) stream_ID.add(ID6);

            // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Cymbol.g:27:20: ( superClass )?
            int alt2=2;
            int LA2_0 = input.LA(1);

            if ( (LA2_0==24) ) {
                alt2=1;
            }
            switch (alt2) {
                case 1 :
                    // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Cymbol.g:27:20: superClass
                    {
                    pushFollow(FOLLOW_superClass_in_classDefinition175);
                    superClass7=superClass();

                    state._fsp--;
                    if (state.failed) return retval;
                    if ( state.backtracking==0 ) stream_superClass.add(superClass7.getTree());

                    }
                    break;

            }

            char_literal8=(Token)match(input,21,FOLLOW_21_in_classDefinition178); if (state.failed) return retval; 
            if ( state.backtracking==0 ) stream_21.add(char_literal8);

            // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Cymbol.g:27:36: ( classMember )+
            int cnt3=0;
            loop3:
            do {
                int alt3=2;
                int LA3_0 = input.LA(1);

                if ( (LA3_0==ID||LA3_0==25||(LA3_0>=29 && LA3_0<=31)) ) {
                    alt3=1;
                }


                switch (alt3) {
            	case 1 :
            	    // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Cymbol.g:27:36: classMember
            	    {
            	    pushFollow(FOLLOW_classMember_in_classDefinition180);
            	    classMember9=classMember();

            	    state._fsp--;
            	    if (state.failed) return retval;
            	    if ( state.backtracking==0 ) stream_classMember.add(classMember9.getTree());

            	    }
            	    break;

            	default :
            	    if ( cnt3 >= 1 ) break loop3;
            	    if (state.backtracking>0) {state.failed=true; return retval;}
                        EarlyExitException eee =
                            new EarlyExitException(3, input);
                        throw eee;
                }
                cnt3++;
            } while (true);

            char_literal10=(Token)match(input,22,FOLLOW_22_in_classDefinition183); if (state.failed) return retval; 
            if ( state.backtracking==0 ) stream_22.add(char_literal10);

            char_literal11=(Token)match(input,23,FOLLOW_23_in_classDefinition185); if (state.failed) return retval; 
            if ( state.backtracking==0 ) stream_23.add(char_literal11);



            // AST REWRITE
            // elements: superClass, ID, 20, classMember
            // token labels: 
            // rule labels: retval
            // token list labels: 
            // rule list labels: 
            // wildcard labels: 
            if ( state.backtracking==0 ) {
            retval.tree = root_0;
            RewriteRuleSubtreeStream stream_retval=new RewriteRuleSubtreeStream(adaptor,"rule retval",retval!=null?retval.tree:null);

            root_0 = (CymbolAST)adaptor.nil();
            // 28:9: -> ^( 'class' ID ( superClass )? ^( MEMBERS ( classMember )+ ) )
            {
                // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Cymbol.g:28:12: ^( 'class' ID ( superClass )? ^( MEMBERS ( classMember )+ ) )
                {
                CymbolAST root_1 = (CymbolAST)adaptor.nil();
                root_1 = (CymbolAST)adaptor.becomeRoot(stream_20.nextNode(), root_1);

                adaptor.addChild(root_1, stream_ID.nextNode());
                // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Cymbol.g:28:25: ( superClass )?
                if ( stream_superClass.hasNext() ) {
                    adaptor.addChild(root_1, stream_superClass.nextTree());

                }
                stream_superClass.reset();
                // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Cymbol.g:28:37: ^( MEMBERS ( classMember )+ )
                {
                CymbolAST root_2 = (CymbolAST)adaptor.nil();
                root_2 = (CymbolAST)adaptor.becomeRoot((CymbolAST)adaptor.create(MEMBERS, "MEMBERS"), root_2);

                if ( !(stream_classMember.hasNext()) ) {
                    throw new RewriteEarlyExitException();
                }
                while ( stream_classMember.hasNext() ) {
                    adaptor.addChild(root_2, stream_classMember.nextTree());

                }
                stream_classMember.reset();

                adaptor.addChild(root_1, root_2);
                }

                adaptor.addChild(root_0, root_1);
                }

            }

            retval.tree = root_0;}
            }

            retval.stop = input.LT(-1);

            if ( state.backtracking==0 ) {

            retval.tree = (CymbolAST)adaptor.rulePostProcessing(root_0);
            adaptor.setTokenBoundaries(retval.tree, retval.start, retval.stop);
            }
        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
    	retval.tree = (CymbolAST)adaptor.errorNode(input, retval.start, input.LT(-1), re);

        }
        finally {
        }
        return retval;
    }
    // $ANTLR end "classDefinition"

    public static class superClass_return extends ParserRuleReturnScope {
        CymbolAST tree;
        public Object getTree() { return tree; }
    };

    // $ANTLR start "superClass"
    // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Cymbol.g:30:1: superClass : ':' 'public' ID -> ^( EXTENDS ID ) ;
    public final CymbolParser.superClass_return superClass() throws RecognitionException {
        CymbolParser.superClass_return retval = new CymbolParser.superClass_return();
        retval.start = input.LT(1);

        CymbolAST root_0 = null;

        Token char_literal12=null;
        Token string_literal13=null;
        Token ID14=null;

        CymbolAST char_literal12_tree=null;
        CymbolAST string_literal13_tree=null;
        CymbolAST ID14_tree=null;
        RewriteRuleTokenStream stream_24=new RewriteRuleTokenStream(adaptor,"token 24");
        RewriteRuleTokenStream stream_25=new RewriteRuleTokenStream(adaptor,"token 25");
        RewriteRuleTokenStream stream_ID=new RewriteRuleTokenStream(adaptor,"token ID");

        try {
            // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Cymbol.g:31:2: ( ':' 'public' ID -> ^( EXTENDS ID ) )
            // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Cymbol.g:31:4: ':' 'public' ID
            {
            char_literal12=(Token)match(input,24,FOLLOW_24_in_superClass224); if (state.failed) return retval; 
            if ( state.backtracking==0 ) stream_24.add(char_literal12);

            string_literal13=(Token)match(input,25,FOLLOW_25_in_superClass226); if (state.failed) return retval; 
            if ( state.backtracking==0 ) stream_25.add(string_literal13);

            ID14=(Token)match(input,ID,FOLLOW_ID_in_superClass228); if (state.failed) return retval; 
            if ( state.backtracking==0 ) stream_ID.add(ID14);



            // AST REWRITE
            // elements: ID
            // token labels: 
            // rule labels: retval
            // token list labels: 
            // rule list labels: 
            // wildcard labels: 
            if ( state.backtracking==0 ) {
            retval.tree = root_0;
            RewriteRuleSubtreeStream stream_retval=new RewriteRuleSubtreeStream(adaptor,"rule retval",retval!=null?retval.tree:null);

            root_0 = (CymbolAST)adaptor.nil();
            // 31:20: -> ^( EXTENDS ID )
            {
                // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Cymbol.g:31:23: ^( EXTENDS ID )
                {
                CymbolAST root_1 = (CymbolAST)adaptor.nil();
                root_1 = (CymbolAST)adaptor.becomeRoot((CymbolAST)adaptor.create(EXTENDS, "EXTENDS"), root_1);

                adaptor.addChild(root_1, stream_ID.nextNode());

                adaptor.addChild(root_0, root_1);
                }

            }

            retval.tree = root_0;}
            }

            retval.stop = input.LT(-1);

            if ( state.backtracking==0 ) {

            retval.tree = (CymbolAST)adaptor.rulePostProcessing(root_0);
            adaptor.setTokenBoundaries(retval.tree, retval.start, retval.stop);
            }
        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
    	retval.tree = (CymbolAST)adaptor.errorNode(input, retval.start, input.LT(-1), re);

        }
        finally {
        }
        return retval;
    }
    // $ANTLR end "superClass"

    public static class classMember_return extends ParserRuleReturnScope {
        CymbolAST tree;
        public Object getTree() { return tree; }
    };

    // $ANTLR start "classMember"
    // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Cymbol.g:35:1: classMember : ( type ID ( '=' expression )? ';' -> ^( FIELD_DECL type ID ( expression )? ) | methodDeclaration | 'public' ':' ->);
    public final CymbolParser.classMember_return classMember() throws RecognitionException {
        CymbolParser.classMember_return retval = new CymbolParser.classMember_return();
        retval.start = input.LT(1);

        CymbolAST root_0 = null;

        Token ID16=null;
        Token char_literal17=null;
        Token char_literal19=null;
        Token string_literal21=null;
        Token char_literal22=null;
        CymbolParser.type_return type15 = null;

        CymbolParser.expression_return expression18 = null;

        CymbolParser.methodDeclaration_return methodDeclaration20 = null;


        CymbolAST ID16_tree=null;
        CymbolAST char_literal17_tree=null;
        CymbolAST char_literal19_tree=null;
        CymbolAST string_literal21_tree=null;
        CymbolAST char_literal22_tree=null;
        RewriteRuleTokenStream stream_24=new RewriteRuleTokenStream(adaptor,"token 24");
        RewriteRuleTokenStream stream_23=new RewriteRuleTokenStream(adaptor,"token 23");
        RewriteRuleTokenStream stream_25=new RewriteRuleTokenStream(adaptor,"token 25");
        RewriteRuleTokenStream stream_ASSIGN=new RewriteRuleTokenStream(adaptor,"token ASSIGN");
        RewriteRuleTokenStream stream_ID=new RewriteRuleTokenStream(adaptor,"token ID");
        RewriteRuleSubtreeStream stream_type=new RewriteRuleSubtreeStream(adaptor,"rule type");
        RewriteRuleSubtreeStream stream_expression=new RewriteRuleSubtreeStream(adaptor,"rule expression");
        try {
            // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Cymbol.g:36:2: ( type ID ( '=' expression )? ';' -> ^( FIELD_DECL type ID ( expression )? ) | methodDeclaration | 'public' ':' ->)
            int alt5=3;
            int LA5_0 = input.LA(1);

            if ( (LA5_0==ID||(LA5_0>=29 && LA5_0<=31)) ) {
                int LA5_1 = input.LA(2);

                if ( (LA5_1==ID) ) {
                    int LA5_3 = input.LA(3);

                    if ( (LA5_3==26) ) {
                        alt5=2;
                    }
                    else if ( (LA5_3==ASSIGN||LA5_3==23) ) {
                        alt5=1;
                    }
                    else {
                        if (state.backtracking>0) {state.failed=true; return retval;}
                        NoViableAltException nvae =
                            new NoViableAltException("", 5, 3, input);

                        throw nvae;
                    }
                }
                else {
                    if (state.backtracking>0) {state.failed=true; return retval;}
                    NoViableAltException nvae =
                        new NoViableAltException("", 5, 1, input);

                    throw nvae;
                }
            }
            else if ( (LA5_0==25) ) {
                alt5=3;
            }
            else {
                if (state.backtracking>0) {state.failed=true; return retval;}
                NoViableAltException nvae =
                    new NoViableAltException("", 5, 0, input);

                throw nvae;
            }
            switch (alt5) {
                case 1 :
                    // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Cymbol.g:36:4: type ID ( '=' expression )? ';'
                    {
                    pushFollow(FOLLOW_type_in_classMember248);
                    type15=type();

                    state._fsp--;
                    if (state.failed) return retval;
                    if ( state.backtracking==0 ) stream_type.add(type15.getTree());
                    ID16=(Token)match(input,ID,FOLLOW_ID_in_classMember250); if (state.failed) return retval; 
                    if ( state.backtracking==0 ) stream_ID.add(ID16);

                    // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Cymbol.g:36:12: ( '=' expression )?
                    int alt4=2;
                    int LA4_0 = input.LA(1);

                    if ( (LA4_0==ASSIGN) ) {
                        alt4=1;
                    }
                    switch (alt4) {
                        case 1 :
                            // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Cymbol.g:36:13: '=' expression
                            {
                            char_literal17=(Token)match(input,ASSIGN,FOLLOW_ASSIGN_in_classMember253); if (state.failed) return retval; 
                            if ( state.backtracking==0 ) stream_ASSIGN.add(char_literal17);

                            pushFollow(FOLLOW_expression_in_classMember255);
                            expression18=expression();

                            state._fsp--;
                            if (state.failed) return retval;
                            if ( state.backtracking==0 ) stream_expression.add(expression18.getTree());

                            }
                            break;

                    }

                    char_literal19=(Token)match(input,23,FOLLOW_23_in_classMember259); if (state.failed) return retval; 
                    if ( state.backtracking==0 ) stream_23.add(char_literal19);



                    // AST REWRITE
                    // elements: expression, ID, type
                    // token labels: 
                    // rule labels: retval
                    // token list labels: 
                    // rule list labels: 
                    // wildcard labels: 
                    if ( state.backtracking==0 ) {
                    retval.tree = root_0;
                    RewriteRuleSubtreeStream stream_retval=new RewriteRuleSubtreeStream(adaptor,"rule retval",retval!=null?retval.tree:null);

                    root_0 = (CymbolAST)adaptor.nil();
                    // 36:34: -> ^( FIELD_DECL type ID ( expression )? )
                    {
                        // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Cymbol.g:36:37: ^( FIELD_DECL type ID ( expression )? )
                        {
                        CymbolAST root_1 = (CymbolAST)adaptor.nil();
                        root_1 = (CymbolAST)adaptor.becomeRoot((CymbolAST)adaptor.create(FIELD_DECL, "FIELD_DECL"), root_1);

                        adaptor.addChild(root_1, stream_type.nextTree());
                        adaptor.addChild(root_1, stream_ID.nextNode());
                        // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Cymbol.g:36:58: ( expression )?
                        if ( stream_expression.hasNext() ) {
                            adaptor.addChild(root_1, stream_expression.nextTree());

                        }
                        stream_expression.reset();

                        adaptor.addChild(root_0, root_1);
                        }

                    }

                    retval.tree = root_0;}
                    }
                    break;
                case 2 :
                    // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Cymbol.g:37:4: methodDeclaration
                    {
                    root_0 = (CymbolAST)adaptor.nil();

                    pushFollow(FOLLOW_methodDeclaration_in_classMember277);
                    methodDeclaration20=methodDeclaration();

                    state._fsp--;
                    if (state.failed) return retval;
                    if ( state.backtracking==0 ) adaptor.addChild(root_0, methodDeclaration20.getTree());

                    }
                    break;
                case 3 :
                    // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Cymbol.g:38:4: 'public' ':'
                    {
                    string_literal21=(Token)match(input,25,FOLLOW_25_in_classMember282); if (state.failed) return retval; 
                    if ( state.backtracking==0 ) stream_25.add(string_literal21);

                    char_literal22=(Token)match(input,24,FOLLOW_24_in_classMember284); if (state.failed) return retval; 
                    if ( state.backtracking==0 ) stream_24.add(char_literal22);



                    // AST REWRITE
                    // elements: 
                    // token labels: 
                    // rule labels: retval
                    // token list labels: 
                    // rule list labels: 
                    // wildcard labels: 
                    if ( state.backtracking==0 ) {
                    retval.tree = root_0;
                    RewriteRuleSubtreeStream stream_retval=new RewriteRuleSubtreeStream(adaptor,"rule retval",retval!=null?retval.tree:null);

                    root_0 = (CymbolAST)adaptor.nil();
                    // 38:17: ->
                    {
                        root_0 = null;
                    }

                    retval.tree = root_0;}
                    }
                    break;

            }
            retval.stop = input.LT(-1);

            if ( state.backtracking==0 ) {

            retval.tree = (CymbolAST)adaptor.rulePostProcessing(root_0);
            adaptor.setTokenBoundaries(retval.tree, retval.start, retval.stop);
            }
        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
    	retval.tree = (CymbolAST)adaptor.errorNode(input, retval.start, input.LT(-1), re);

        }
        finally {
        }
        return retval;
    }
    // $ANTLR end "classMember"

    public static class methodDeclaration_return extends ParserRuleReturnScope {
        CymbolAST tree;
        public Object getTree() { return tree; }
    };

    // $ANTLR start "methodDeclaration"
    // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Cymbol.g:42:1: methodDeclaration : type ID '(' ( formalParameters )? ')' block -> ^( METHOD_DECL type ID ( formalParameters )? block ) ;
    public final CymbolParser.methodDeclaration_return methodDeclaration() throws RecognitionException {
        CymbolParser.methodDeclaration_return retval = new CymbolParser.methodDeclaration_return();
        retval.start = input.LT(1);

        CymbolAST root_0 = null;

        Token ID24=null;
        Token char_literal25=null;
        Token char_literal27=null;
        CymbolParser.type_return type23 = null;

        CymbolParser.formalParameters_return formalParameters26 = null;

        CymbolParser.block_return block28 = null;


        CymbolAST ID24_tree=null;
        CymbolAST char_literal25_tree=null;
        CymbolAST char_literal27_tree=null;
        RewriteRuleTokenStream stream_26=new RewriteRuleTokenStream(adaptor,"token 26");
        RewriteRuleTokenStream stream_27=new RewriteRuleTokenStream(adaptor,"token 27");
        RewriteRuleTokenStream stream_ID=new RewriteRuleTokenStream(adaptor,"token ID");
        RewriteRuleSubtreeStream stream_formalParameters=new RewriteRuleSubtreeStream(adaptor,"rule formalParameters");
        RewriteRuleSubtreeStream stream_type=new RewriteRuleSubtreeStream(adaptor,"rule type");
        RewriteRuleSubtreeStream stream_block=new RewriteRuleSubtreeStream(adaptor,"rule block");
        try {
            // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Cymbol.g:43:5: ( type ID '(' ( formalParameters )? ')' block -> ^( METHOD_DECL type ID ( formalParameters )? block ) )
            // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Cymbol.g:43:9: type ID '(' ( formalParameters )? ')' block
            {
            pushFollow(FOLLOW_type_in_methodDeclaration305);
            type23=type();

            state._fsp--;
            if (state.failed) return retval;
            if ( state.backtracking==0 ) stream_type.add(type23.getTree());
            ID24=(Token)match(input,ID,FOLLOW_ID_in_methodDeclaration307); if (state.failed) return retval; 
            if ( state.backtracking==0 ) stream_ID.add(ID24);

            char_literal25=(Token)match(input,26,FOLLOW_26_in_methodDeclaration309); if (state.failed) return retval; 
            if ( state.backtracking==0 ) stream_26.add(char_literal25);

            // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Cymbol.g:43:21: ( formalParameters )?
            int alt6=2;
            int LA6_0 = input.LA(1);

            if ( (LA6_0==ID||(LA6_0>=29 && LA6_0<=31)) ) {
                alt6=1;
            }
            switch (alt6) {
                case 1 :
                    // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Cymbol.g:43:21: formalParameters
                    {
                    pushFollow(FOLLOW_formalParameters_in_methodDeclaration311);
                    formalParameters26=formalParameters();

                    state._fsp--;
                    if (state.failed) return retval;
                    if ( state.backtracking==0 ) stream_formalParameters.add(formalParameters26.getTree());

                    }
                    break;

            }

            char_literal27=(Token)match(input,27,FOLLOW_27_in_methodDeclaration314); if (state.failed) return retval; 
            if ( state.backtracking==0 ) stream_27.add(char_literal27);

            pushFollow(FOLLOW_block_in_methodDeclaration316);
            block28=block();

            state._fsp--;
            if (state.failed) return retval;
            if ( state.backtracking==0 ) stream_block.add(block28.getTree());


            // AST REWRITE
            // elements: ID, formalParameters, type, block
            // token labels: 
            // rule labels: retval
            // token list labels: 
            // rule list labels: 
            // wildcard labels: 
            if ( state.backtracking==0 ) {
            retval.tree = root_0;
            RewriteRuleSubtreeStream stream_retval=new RewriteRuleSubtreeStream(adaptor,"rule retval",retval!=null?retval.tree:null);

            root_0 = (CymbolAST)adaptor.nil();
            // 44:9: -> ^( METHOD_DECL type ID ( formalParameters )? block )
            {
                // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Cymbol.g:44:12: ^( METHOD_DECL type ID ( formalParameters )? block )
                {
                CymbolAST root_1 = (CymbolAST)adaptor.nil();
                root_1 = (CymbolAST)adaptor.becomeRoot((CymbolAST)adaptor.create(METHOD_DECL, "METHOD_DECL"), root_1);

                adaptor.addChild(root_1, stream_type.nextTree());
                adaptor.addChild(root_1, stream_ID.nextNode());
                // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Cymbol.g:44:34: ( formalParameters )?
                if ( stream_formalParameters.hasNext() ) {
                    adaptor.addChild(root_1, stream_formalParameters.nextTree());

                }
                stream_formalParameters.reset();
                adaptor.addChild(root_1, stream_block.nextTree());

                adaptor.addChild(root_0, root_1);
                }

            }

            retval.tree = root_0;}
            }

            retval.stop = input.LT(-1);

            if ( state.backtracking==0 ) {

            retval.tree = (CymbolAST)adaptor.rulePostProcessing(root_0);
            adaptor.setTokenBoundaries(retval.tree, retval.start, retval.stop);
            }
        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
    	retval.tree = (CymbolAST)adaptor.errorNode(input, retval.start, input.LT(-1), re);

        }
        finally {
        }
        return retval;
    }
    // $ANTLR end "methodDeclaration"

    public static class formalParameters_return extends ParserRuleReturnScope {
        CymbolAST tree;
        public Object getTree() { return tree; }
    };

    // $ANTLR start "formalParameters"
    // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Cymbol.g:48:1: formalParameters : type ID ( ',' type ID )* -> ( ^( ARG_DECL type ID ) )+ ;
    public final CymbolParser.formalParameters_return formalParameters() throws RecognitionException {
        CymbolParser.formalParameters_return retval = new CymbolParser.formalParameters_return();
        retval.start = input.LT(1);

        CymbolAST root_0 = null;

        Token ID30=null;
        Token char_literal31=null;
        Token ID33=null;
        CymbolParser.type_return type29 = null;

        CymbolParser.type_return type32 = null;


        CymbolAST ID30_tree=null;
        CymbolAST char_literal31_tree=null;
        CymbolAST ID33_tree=null;
        RewriteRuleTokenStream stream_28=new RewriteRuleTokenStream(adaptor,"token 28");
        RewriteRuleTokenStream stream_ID=new RewriteRuleTokenStream(adaptor,"token ID");
        RewriteRuleSubtreeStream stream_type=new RewriteRuleSubtreeStream(adaptor,"rule type");
        try {
            // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Cymbol.g:49:5: ( type ID ( ',' type ID )* -> ( ^( ARG_DECL type ID ) )+ )
            // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Cymbol.g:49:9: type ID ( ',' type ID )*
            {
            pushFollow(FOLLOW_type_in_formalParameters359);
            type29=type();

            state._fsp--;
            if (state.failed) return retval;
            if ( state.backtracking==0 ) stream_type.add(type29.getTree());
            ID30=(Token)match(input,ID,FOLLOW_ID_in_formalParameters361); if (state.failed) return retval; 
            if ( state.backtracking==0 ) stream_ID.add(ID30);

            // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Cymbol.g:49:17: ( ',' type ID )*
            loop7:
            do {
                int alt7=2;
                int LA7_0 = input.LA(1);

                if ( (LA7_0==28) ) {
                    alt7=1;
                }


                switch (alt7) {
            	case 1 :
            	    // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Cymbol.g:49:18: ',' type ID
            	    {
            	    char_literal31=(Token)match(input,28,FOLLOW_28_in_formalParameters364); if (state.failed) return retval; 
            	    if ( state.backtracking==0 ) stream_28.add(char_literal31);

            	    pushFollow(FOLLOW_type_in_formalParameters366);
            	    type32=type();

            	    state._fsp--;
            	    if (state.failed) return retval;
            	    if ( state.backtracking==0 ) stream_type.add(type32.getTree());
            	    ID33=(Token)match(input,ID,FOLLOW_ID_in_formalParameters368); if (state.failed) return retval; 
            	    if ( state.backtracking==0 ) stream_ID.add(ID33);


            	    }
            	    break;

            	default :
            	    break loop7;
                }
            } while (true);



            // AST REWRITE
            // elements: ID, type
            // token labels: 
            // rule labels: retval
            // token list labels: 
            // rule list labels: 
            // wildcard labels: 
            if ( state.backtracking==0 ) {
            retval.tree = root_0;
            RewriteRuleSubtreeStream stream_retval=new RewriteRuleSubtreeStream(adaptor,"rule retval",retval!=null?retval.tree:null);

            root_0 = (CymbolAST)adaptor.nil();
            // 49:32: -> ( ^( ARG_DECL type ID ) )+
            {
                if ( !(stream_ID.hasNext()||stream_type.hasNext()) ) {
                    throw new RewriteEarlyExitException();
                }
                while ( stream_ID.hasNext()||stream_type.hasNext() ) {
                    // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Cymbol.g:49:35: ^( ARG_DECL type ID )
                    {
                    CymbolAST root_1 = (CymbolAST)adaptor.nil();
                    root_1 = (CymbolAST)adaptor.becomeRoot((CymbolAST)adaptor.create(ARG_DECL, "ARG_DECL"), root_1);

                    adaptor.addChild(root_1, stream_type.nextTree());
                    adaptor.addChild(root_1, stream_ID.nextNode());

                    adaptor.addChild(root_0, root_1);
                    }

                }
                stream_ID.reset();
                stream_type.reset();

            }

            retval.tree = root_0;}
            }

            retval.stop = input.LT(-1);

            if ( state.backtracking==0 ) {

            retval.tree = (CymbolAST)adaptor.rulePostProcessing(root_0);
            adaptor.setTokenBoundaries(retval.tree, retval.start, retval.stop);
            }
        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
    	retval.tree = (CymbolAST)adaptor.errorNode(input, retval.start, input.LT(-1), re);

        }
        finally {
        }
        return retval;
    }
    // $ANTLR end "formalParameters"

    public static class type_return extends ParserRuleReturnScope {
        CymbolAST tree;
        public Object getTree() { return tree; }
    };

    // $ANTLR start "type"
    // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Cymbol.g:52:1: type : ( 'float' | 'int' | 'void' | ID );
    public final CymbolParser.type_return type() throws RecognitionException {
        CymbolParser.type_return retval = new CymbolParser.type_return();
        retval.start = input.LT(1);

        CymbolAST root_0 = null;

        Token set34=null;

        CymbolAST set34_tree=null;

        try {
            // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Cymbol.g:52:5: ( 'float' | 'int' | 'void' | ID )
            // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Cymbol.g:
            {
            root_0 = (CymbolAST)adaptor.nil();

            set34=(Token)input.LT(1);
            if ( input.LA(1)==ID||(input.LA(1)>=29 && input.LA(1)<=31) ) {
                input.consume();
                if ( state.backtracking==0 ) adaptor.addChild(root_0, (CymbolAST)adaptor.create(set34));
                state.errorRecovery=false;state.failed=false;
            }
            else {
                if (state.backtracking>0) {state.failed=true; return retval;}
                MismatchedSetException mse = new MismatchedSetException(null,input);
                throw mse;
            }


            }

            retval.stop = input.LT(-1);

            if ( state.backtracking==0 ) {

            retval.tree = (CymbolAST)adaptor.rulePostProcessing(root_0);
            adaptor.setTokenBoundaries(retval.tree, retval.start, retval.stop);
            }
        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
    	retval.tree = (CymbolAST)adaptor.errorNode(input, retval.start, input.LT(-1), re);

        }
        finally {
        }
        return retval;
    }
    // $ANTLR end "type"

    public static class block_return extends ParserRuleReturnScope {
        CymbolAST tree;
        public Object getTree() { return tree; }
    };

    // $ANTLR start "block"
    // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Cymbol.g:59:1: block : '{' ( statement )* '}' -> ^( BLOCK ( statement )* ) ;
    public final CymbolParser.block_return block() throws RecognitionException {
        CymbolParser.block_return retval = new CymbolParser.block_return();
        retval.start = input.LT(1);

        CymbolAST root_0 = null;

        Token char_literal35=null;
        Token char_literal37=null;
        CymbolParser.statement_return statement36 = null;


        CymbolAST char_literal35_tree=null;
        CymbolAST char_literal37_tree=null;
        RewriteRuleTokenStream stream_21=new RewriteRuleTokenStream(adaptor,"token 21");
        RewriteRuleTokenStream stream_22=new RewriteRuleTokenStream(adaptor,"token 22");
        RewriteRuleSubtreeStream stream_statement=new RewriteRuleSubtreeStream(adaptor,"rule statement");
        try {
            // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Cymbol.g:60:5: ( '{' ( statement )* '}' -> ^( BLOCK ( statement )* ) )
            // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Cymbol.g:60:9: '{' ( statement )* '}'
            {
            char_literal35=(Token)match(input,21,FOLLOW_21_in_block442); if (state.failed) return retval; 
            if ( state.backtracking==0 ) stream_21.add(char_literal35);

            // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Cymbol.g:60:13: ( statement )*
            loop8:
            do {
                int alt8=2;
                int LA8_0 = input.LA(1);

                if ( ((LA8_0>=ID && LA8_0<=INT)||LA8_0==21||LA8_0==23||LA8_0==26||(LA8_0>=29 && LA8_0<=32)||(LA8_0>=35 && LA8_0<=36)) ) {
                    alt8=1;
                }


                switch (alt8) {
            	case 1 :
            	    // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Cymbol.g:60:13: statement
            	    {
            	    pushFollow(FOLLOW_statement_in_block444);
            	    statement36=statement();

            	    state._fsp--;
            	    if (state.failed) return retval;
            	    if ( state.backtracking==0 ) stream_statement.add(statement36.getTree());

            	    }
            	    break;

            	default :
            	    break loop8;
                }
            } while (true);

            char_literal37=(Token)match(input,22,FOLLOW_22_in_block447); if (state.failed) return retval; 
            if ( state.backtracking==0 ) stream_22.add(char_literal37);



            // AST REWRITE
            // elements: statement
            // token labels: 
            // rule labels: retval
            // token list labels: 
            // rule list labels: 
            // wildcard labels: 
            if ( state.backtracking==0 ) {
            retval.tree = root_0;
            RewriteRuleSubtreeStream stream_retval=new RewriteRuleSubtreeStream(adaptor,"rule retval",retval!=null?retval.tree:null);

            root_0 = (CymbolAST)adaptor.nil();
            // 60:28: -> ^( BLOCK ( statement )* )
            {
                // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Cymbol.g:60:31: ^( BLOCK ( statement )* )
                {
                CymbolAST root_1 = (CymbolAST)adaptor.nil();
                root_1 = (CymbolAST)adaptor.becomeRoot((CymbolAST)adaptor.create(BLOCK, "BLOCK"), root_1);

                // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Cymbol.g:60:39: ( statement )*
                while ( stream_statement.hasNext() ) {
                    adaptor.addChild(root_1, stream_statement.nextTree());

                }
                stream_statement.reset();

                adaptor.addChild(root_0, root_1);
                }

            }

            retval.tree = root_0;}
            }

            retval.stop = input.LT(-1);

            if ( state.backtracking==0 ) {

            retval.tree = (CymbolAST)adaptor.rulePostProcessing(root_0);
            adaptor.setTokenBoundaries(retval.tree, retval.start, retval.stop);
            }
        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
    	retval.tree = (CymbolAST)adaptor.errorNode(input, retval.start, input.LT(-1), re);

        }
        finally {
        }
        return retval;
    }
    // $ANTLR end "block"

    public static class varDeclaration_return extends ParserRuleReturnScope {
        CymbolAST tree;
        public Object getTree() { return tree; }
    };

    // $ANTLR start "varDeclaration"
    // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Cymbol.g:65:1: varDeclaration : type ID ( '=' expression )? ';' -> ^( VAR_DECL type ID ( expression )? ) ;
    public final CymbolParser.varDeclaration_return varDeclaration() throws RecognitionException {
        CymbolParser.varDeclaration_return retval = new CymbolParser.varDeclaration_return();
        retval.start = input.LT(1);

        CymbolAST root_0 = null;

        Token ID39=null;
        Token char_literal40=null;
        Token char_literal42=null;
        CymbolParser.type_return type38 = null;

        CymbolParser.expression_return expression41 = null;


        CymbolAST ID39_tree=null;
        CymbolAST char_literal40_tree=null;
        CymbolAST char_literal42_tree=null;
        RewriteRuleTokenStream stream_23=new RewriteRuleTokenStream(adaptor,"token 23");
        RewriteRuleTokenStream stream_ASSIGN=new RewriteRuleTokenStream(adaptor,"token ASSIGN");
        RewriteRuleTokenStream stream_ID=new RewriteRuleTokenStream(adaptor,"token ID");
        RewriteRuleSubtreeStream stream_type=new RewriteRuleSubtreeStream(adaptor,"rule type");
        RewriteRuleSubtreeStream stream_expression=new RewriteRuleSubtreeStream(adaptor,"rule expression");
        try {
            // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Cymbol.g:66:5: ( type ID ( '=' expression )? ';' -> ^( VAR_DECL type ID ( expression )? ) )
            // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Cymbol.g:66:9: type ID ( '=' expression )? ';'
            {
            pushFollow(FOLLOW_type_in_varDeclaration477);
            type38=type();

            state._fsp--;
            if (state.failed) return retval;
            if ( state.backtracking==0 ) stream_type.add(type38.getTree());
            ID39=(Token)match(input,ID,FOLLOW_ID_in_varDeclaration479); if (state.failed) return retval; 
            if ( state.backtracking==0 ) stream_ID.add(ID39);

            // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Cymbol.g:66:17: ( '=' expression )?
            int alt9=2;
            int LA9_0 = input.LA(1);

            if ( (LA9_0==ASSIGN) ) {
                alt9=1;
            }
            switch (alt9) {
                case 1 :
                    // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Cymbol.g:66:18: '=' expression
                    {
                    char_literal40=(Token)match(input,ASSIGN,FOLLOW_ASSIGN_in_varDeclaration482); if (state.failed) return retval; 
                    if ( state.backtracking==0 ) stream_ASSIGN.add(char_literal40);

                    pushFollow(FOLLOW_expression_in_varDeclaration484);
                    expression41=expression();

                    state._fsp--;
                    if (state.failed) return retval;
                    if ( state.backtracking==0 ) stream_expression.add(expression41.getTree());

                    }
                    break;

            }

            char_literal42=(Token)match(input,23,FOLLOW_23_in_varDeclaration488); if (state.failed) return retval; 
            if ( state.backtracking==0 ) stream_23.add(char_literal42);



            // AST REWRITE
            // elements: ID, type, expression
            // token labels: 
            // rule labels: retval
            // token list labels: 
            // rule list labels: 
            // wildcard labels: 
            if ( state.backtracking==0 ) {
            retval.tree = root_0;
            RewriteRuleSubtreeStream stream_retval=new RewriteRuleSubtreeStream(adaptor,"rule retval",retval!=null?retval.tree:null);

            root_0 = (CymbolAST)adaptor.nil();
            // 66:39: -> ^( VAR_DECL type ID ( expression )? )
            {
                // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Cymbol.g:66:42: ^( VAR_DECL type ID ( expression )? )
                {
                CymbolAST root_1 = (CymbolAST)adaptor.nil();
                root_1 = (CymbolAST)adaptor.becomeRoot((CymbolAST)adaptor.create(VAR_DECL, "VAR_DECL"), root_1);

                adaptor.addChild(root_1, stream_type.nextTree());
                adaptor.addChild(root_1, stream_ID.nextNode());
                // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Cymbol.g:66:61: ( expression )?
                if ( stream_expression.hasNext() ) {
                    adaptor.addChild(root_1, stream_expression.nextTree());

                }
                stream_expression.reset();

                adaptor.addChild(root_0, root_1);
                }

            }

            retval.tree = root_0;}
            }

            retval.stop = input.LT(-1);

            if ( state.backtracking==0 ) {

            retval.tree = (CymbolAST)adaptor.rulePostProcessing(root_0);
            adaptor.setTokenBoundaries(retval.tree, retval.start, retval.stop);
            }
        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
    	retval.tree = (CymbolAST)adaptor.errorNode(input, retval.start, input.LT(-1), re);

        }
        finally {
        }
        return retval;
    }
    // $ANTLR end "varDeclaration"

    public static class statement_return extends ParserRuleReturnScope {
        CymbolAST tree;
        public Object getTree() { return tree; }
    };

    // $ANTLR start "statement"
    // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Cymbol.g:70:1: statement : ( block | varDeclaration | 'return' ( expression )? ';' -> ^( 'return' ( expression )? ) | postfixExpression ( '=' expression -> ^( '=' postfixExpression expression ) | -> ^( EXPR postfixExpression ) ) ';' | ';' ->);
    public final CymbolParser.statement_return statement() throws RecognitionException {
        CymbolParser.statement_return retval = new CymbolParser.statement_return();
        retval.start = input.LT(1);

        CymbolAST root_0 = null;

        Token string_literal45=null;
        Token char_literal47=null;
        Token char_literal49=null;
        Token char_literal51=null;
        Token char_literal52=null;
        CymbolParser.block_return block43 = null;

        CymbolParser.varDeclaration_return varDeclaration44 = null;

        CymbolParser.expression_return expression46 = null;

        CymbolParser.postfixExpression_return postfixExpression48 = null;

        CymbolParser.expression_return expression50 = null;


        CymbolAST string_literal45_tree=null;
        CymbolAST char_literal47_tree=null;
        CymbolAST char_literal49_tree=null;
        CymbolAST char_literal51_tree=null;
        CymbolAST char_literal52_tree=null;
        RewriteRuleTokenStream stream_32=new RewriteRuleTokenStream(adaptor,"token 32");
        RewriteRuleTokenStream stream_23=new RewriteRuleTokenStream(adaptor,"token 23");
        RewriteRuleTokenStream stream_ASSIGN=new RewriteRuleTokenStream(adaptor,"token ASSIGN");
        RewriteRuleSubtreeStream stream_expression=new RewriteRuleSubtreeStream(adaptor,"rule expression");
        RewriteRuleSubtreeStream stream_postfixExpression=new RewriteRuleSubtreeStream(adaptor,"rule postfixExpression");
        try {
            // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Cymbol.g:71:5: ( block | varDeclaration | 'return' ( expression )? ';' -> ^( 'return' ( expression )? ) | postfixExpression ( '=' expression -> ^( '=' postfixExpression expression ) | -> ^( EXPR postfixExpression ) ) ';' | ';' ->)
            int alt12=5;
            switch ( input.LA(1) ) {
            case 21:
                {
                alt12=1;
                }
                break;
            case ID:
                {
                int LA12_2 = input.LA(2);

                if ( (LA12_2==ASSIGN||LA12_2==23||LA12_2==26||LA12_2==34) ) {
                    alt12=4;
                }
                else if ( (LA12_2==ID) ) {
                    alt12=2;
                }
                else {
                    if (state.backtracking>0) {state.failed=true; return retval;}
                    NoViableAltException nvae =
                        new NoViableAltException("", 12, 2, input);

                    throw nvae;
                }
                }
                break;
            case 32:
                {
                alt12=3;
                }
                break;
            case INT:
            case 26:
            case 35:
            case 36:
                {
                alt12=4;
                }
                break;
            case 29:
            case 30:
            case 31:
                {
                alt12=2;
                }
                break;
            case 23:
                {
                alt12=5;
                }
                break;
            default:
                if (state.backtracking>0) {state.failed=true; return retval;}
                NoViableAltException nvae =
                    new NoViableAltException("", 12, 0, input);

                throw nvae;
            }

            switch (alt12) {
                case 1 :
                    // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Cymbol.g:71:9: block
                    {
                    root_0 = (CymbolAST)adaptor.nil();

                    pushFollow(FOLLOW_block_in_statement521);
                    block43=block();

                    state._fsp--;
                    if (state.failed) return retval;
                    if ( state.backtracking==0 ) adaptor.addChild(root_0, block43.getTree());

                    }
                    break;
                case 2 :
                    // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Cymbol.g:72:7: varDeclaration
                    {
                    root_0 = (CymbolAST)adaptor.nil();

                    pushFollow(FOLLOW_varDeclaration_in_statement529);
                    varDeclaration44=varDeclaration();

                    state._fsp--;
                    if (state.failed) return retval;
                    if ( state.backtracking==0 ) adaptor.addChild(root_0, varDeclaration44.getTree());

                    }
                    break;
                case 3 :
                    // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Cymbol.g:73:9: 'return' ( expression )? ';'
                    {
                    string_literal45=(Token)match(input,32,FOLLOW_32_in_statement539); if (state.failed) return retval; 
                    if ( state.backtracking==0 ) stream_32.add(string_literal45);

                    // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Cymbol.g:73:18: ( expression )?
                    int alt10=2;
                    int LA10_0 = input.LA(1);

                    if ( ((LA10_0>=ID && LA10_0<=INT)||LA10_0==26||(LA10_0>=35 && LA10_0<=36)) ) {
                        alt10=1;
                    }
                    switch (alt10) {
                        case 1 :
                            // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Cymbol.g:73:18: expression
                            {
                            pushFollow(FOLLOW_expression_in_statement541);
                            expression46=expression();

                            state._fsp--;
                            if (state.failed) return retval;
                            if ( state.backtracking==0 ) stream_expression.add(expression46.getTree());

                            }
                            break;

                    }

                    char_literal47=(Token)match(input,23,FOLLOW_23_in_statement544); if (state.failed) return retval; 
                    if ( state.backtracking==0 ) stream_23.add(char_literal47);



                    // AST REWRITE
                    // elements: expression, 32
                    // token labels: 
                    // rule labels: retval
                    // token list labels: 
                    // rule list labels: 
                    // wildcard labels: 
                    if ( state.backtracking==0 ) {
                    retval.tree = root_0;
                    RewriteRuleSubtreeStream stream_retval=new RewriteRuleSubtreeStream(adaptor,"rule retval",retval!=null?retval.tree:null);

                    root_0 = (CymbolAST)adaptor.nil();
                    // 73:34: -> ^( 'return' ( expression )? )
                    {
                        // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Cymbol.g:73:37: ^( 'return' ( expression )? )
                        {
                        CymbolAST root_1 = (CymbolAST)adaptor.nil();
                        root_1 = (CymbolAST)adaptor.becomeRoot(stream_32.nextNode(), root_1);

                        // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Cymbol.g:73:48: ( expression )?
                        if ( stream_expression.hasNext() ) {
                            adaptor.addChild(root_1, stream_expression.nextTree());

                        }
                        stream_expression.reset();

                        adaptor.addChild(root_0, root_1);
                        }

                    }

                    retval.tree = root_0;}
                    }
                    break;
                case 4 :
                    // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Cymbol.g:74:9: postfixExpression ( '=' expression -> ^( '=' postfixExpression expression ) | -> ^( EXPR postfixExpression ) ) ';'
                    {
                    pushFollow(FOLLOW_postfixExpression_in_statement563);
                    postfixExpression48=postfixExpression();

                    state._fsp--;
                    if (state.failed) return retval;
                    if ( state.backtracking==0 ) stream_postfixExpression.add(postfixExpression48.getTree());
                    // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Cymbol.g:75:9: ( '=' expression -> ^( '=' postfixExpression expression ) | -> ^( EXPR postfixExpression ) )
                    int alt11=2;
                    int LA11_0 = input.LA(1);

                    if ( (LA11_0==ASSIGN) ) {
                        alt11=1;
                    }
                    else if ( (LA11_0==23) ) {
                        alt11=2;
                    }
                    else {
                        if (state.backtracking>0) {state.failed=true; return retval;}
                        NoViableAltException nvae =
                            new NoViableAltException("", 11, 0, input);

                        throw nvae;
                    }
                    switch (alt11) {
                        case 1 :
                            // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Cymbol.g:75:13: '=' expression
                            {
                            char_literal49=(Token)match(input,ASSIGN,FOLLOW_ASSIGN_in_statement578); if (state.failed) return retval; 
                            if ( state.backtracking==0 ) stream_ASSIGN.add(char_literal49);

                            pushFollow(FOLLOW_expression_in_statement580);
                            expression50=expression();

                            state._fsp--;
                            if (state.failed) return retval;
                            if ( state.backtracking==0 ) stream_expression.add(expression50.getTree());


                            // AST REWRITE
                            // elements: expression, postfixExpression, ASSIGN
                            // token labels: 
                            // rule labels: retval
                            // token list labels: 
                            // rule list labels: 
                            // wildcard labels: 
                            if ( state.backtracking==0 ) {
                            retval.tree = root_0;
                            RewriteRuleSubtreeStream stream_retval=new RewriteRuleSubtreeStream(adaptor,"rule retval",retval!=null?retval.tree:null);

                            root_0 = (CymbolAST)adaptor.nil();
                            // 75:28: -> ^( '=' postfixExpression expression )
                            {
                                // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Cymbol.g:75:31: ^( '=' postfixExpression expression )
                                {
                                CymbolAST root_1 = (CymbolAST)adaptor.nil();
                                root_1 = (CymbolAST)adaptor.becomeRoot(stream_ASSIGN.nextNode(), root_1);

                                adaptor.addChild(root_1, stream_postfixExpression.nextTree());
                                adaptor.addChild(root_1, stream_expression.nextTree());

                                adaptor.addChild(root_0, root_1);
                                }

                            }

                            retval.tree = root_0;}
                            }
                            break;
                        case 2 :
                            // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Cymbol.g:76:13: 
                            {

                            // AST REWRITE
                            // elements: postfixExpression
                            // token labels: 
                            // rule labels: retval
                            // token list labels: 
                            // rule list labels: 
                            // wildcard labels: 
                            if ( state.backtracking==0 ) {
                            retval.tree = root_0;
                            RewriteRuleSubtreeStream stream_retval=new RewriteRuleSubtreeStream(adaptor,"rule retval",retval!=null?retval.tree:null);

                            root_0 = (CymbolAST)adaptor.nil();
                            // 76:13: -> ^( EXPR postfixExpression )
                            {
                                // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Cymbol.g:76:16: ^( EXPR postfixExpression )
                                {
                                CymbolAST root_1 = (CymbolAST)adaptor.nil();
                                root_1 = (CymbolAST)adaptor.becomeRoot((CymbolAST)adaptor.create(EXPR, "EXPR"), root_1);

                                adaptor.addChild(root_1, stream_postfixExpression.nextTree());

                                adaptor.addChild(root_0, root_1);
                                }

                            }

                            retval.tree = root_0;}
                            }
                            break;

                    }

                    char_literal51=(Token)match(input,23,FOLLOW_23_in_statement630); if (state.failed) return retval; 
                    if ( state.backtracking==0 ) stream_23.add(char_literal51);


                    }
                    break;
                case 5 :
                    // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Cymbol.g:79:7: ';'
                    {
                    char_literal52=(Token)match(input,23,FOLLOW_23_in_statement639); if (state.failed) return retval; 
                    if ( state.backtracking==0 ) stream_23.add(char_literal52);



                    // AST REWRITE
                    // elements: 
                    // token labels: 
                    // rule labels: retval
                    // token list labels: 
                    // rule list labels: 
                    // wildcard labels: 
                    if ( state.backtracking==0 ) {
                    retval.tree = root_0;
                    RewriteRuleSubtreeStream stream_retval=new RewriteRuleSubtreeStream(adaptor,"rule retval",retval!=null?retval.tree:null);

                    root_0 = (CymbolAST)adaptor.nil();
                    // 79:11: ->
                    {
                        root_0 = null;
                    }

                    retval.tree = root_0;}
                    }
                    break;

            }
            retval.stop = input.LT(-1);

            if ( state.backtracking==0 ) {

            retval.tree = (CymbolAST)adaptor.rulePostProcessing(root_0);
            adaptor.setTokenBoundaries(retval.tree, retval.start, retval.stop);
            }
        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
    	retval.tree = (CymbolAST)adaptor.errorNode(input, retval.start, input.LT(-1), re);

        }
        finally {
        }
        return retval;
    }
    // $ANTLR end "statement"

    public static class expressionList_return extends ParserRuleReturnScope {
        CymbolAST tree;
        public Object getTree() { return tree; }
    };

    // $ANTLR start "expressionList"
    // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Cymbol.g:82:1: expressionList : ( expression ( ',' expression )* -> ^( ELIST ( expression )+ ) | -> ELIST );
    public final CymbolParser.expressionList_return expressionList() throws RecognitionException {
        CymbolParser.expressionList_return retval = new CymbolParser.expressionList_return();
        retval.start = input.LT(1);

        CymbolAST root_0 = null;

        Token char_literal54=null;
        CymbolParser.expression_return expression53 = null;

        CymbolParser.expression_return expression55 = null;


        CymbolAST char_literal54_tree=null;
        RewriteRuleTokenStream stream_28=new RewriteRuleTokenStream(adaptor,"token 28");
        RewriteRuleSubtreeStream stream_expression=new RewriteRuleSubtreeStream(adaptor,"rule expression");
        try {
            // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Cymbol.g:83:5: ( expression ( ',' expression )* -> ^( ELIST ( expression )+ ) | -> ELIST )
            int alt14=2;
            int LA14_0 = input.LA(1);

            if ( ((LA14_0>=ID && LA14_0<=INT)||LA14_0==26||(LA14_0>=35 && LA14_0<=36)) ) {
                alt14=1;
            }
            else if ( (LA14_0==27) ) {
                alt14=2;
            }
            else {
                if (state.backtracking>0) {state.failed=true; return retval;}
                NoViableAltException nvae =
                    new NoViableAltException("", 14, 0, input);

                throw nvae;
            }
            switch (alt14) {
                case 1 :
                    // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Cymbol.g:83:9: expression ( ',' expression )*
                    {
                    pushFollow(FOLLOW_expression_in_expressionList661);
                    expression53=expression();

                    state._fsp--;
                    if (state.failed) return retval;
                    if ( state.backtracking==0 ) stream_expression.add(expression53.getTree());
                    // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Cymbol.g:83:20: ( ',' expression )*
                    loop13:
                    do {
                        int alt13=2;
                        int LA13_0 = input.LA(1);

                        if ( (LA13_0==28) ) {
                            alt13=1;
                        }


                        switch (alt13) {
                    	case 1 :
                    	    // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Cymbol.g:83:21: ',' expression
                    	    {
                    	    char_literal54=(Token)match(input,28,FOLLOW_28_in_expressionList664); if (state.failed) return retval; 
                    	    if ( state.backtracking==0 ) stream_28.add(char_literal54);

                    	    pushFollow(FOLLOW_expression_in_expressionList666);
                    	    expression55=expression();

                    	    state._fsp--;
                    	    if (state.failed) return retval;
                    	    if ( state.backtracking==0 ) stream_expression.add(expression55.getTree());

                    	    }
                    	    break;

                    	default :
                    	    break loop13;
                        }
                    } while (true);



                    // AST REWRITE
                    // elements: expression
                    // token labels: 
                    // rule labels: retval
                    // token list labels: 
                    // rule list labels: 
                    // wildcard labels: 
                    if ( state.backtracking==0 ) {
                    retval.tree = root_0;
                    RewriteRuleSubtreeStream stream_retval=new RewriteRuleSubtreeStream(adaptor,"rule retval",retval!=null?retval.tree:null);

                    root_0 = (CymbolAST)adaptor.nil();
                    // 83:38: -> ^( ELIST ( expression )+ )
                    {
                        // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Cymbol.g:83:41: ^( ELIST ( expression )+ )
                        {
                        CymbolAST root_1 = (CymbolAST)adaptor.nil();
                        root_1 = (CymbolAST)adaptor.becomeRoot((CymbolAST)adaptor.create(ELIST, "ELIST"), root_1);

                        if ( !(stream_expression.hasNext()) ) {
                            throw new RewriteEarlyExitException();
                        }
                        while ( stream_expression.hasNext() ) {
                            adaptor.addChild(root_1, stream_expression.nextTree());

                        }
                        stream_expression.reset();

                        adaptor.addChild(root_0, root_1);
                        }

                    }

                    retval.tree = root_0;}
                    }
                    break;
                case 2 :
                    // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Cymbol.g:84:9: 
                    {

                    // AST REWRITE
                    // elements: 
                    // token labels: 
                    // rule labels: retval
                    // token list labels: 
                    // rule list labels: 
                    // wildcard labels: 
                    if ( state.backtracking==0 ) {
                    retval.tree = root_0;
                    RewriteRuleSubtreeStream stream_retval=new RewriteRuleSubtreeStream(adaptor,"rule retval",retval!=null?retval.tree:null);

                    root_0 = (CymbolAST)adaptor.nil();
                    // 84:9: -> ELIST
                    {
                        adaptor.addChild(root_0, (CymbolAST)adaptor.create(ELIST, "ELIST"));

                    }

                    retval.tree = root_0;}
                    }
                    break;

            }
            retval.stop = input.LT(-1);

            if ( state.backtracking==0 ) {

            retval.tree = (CymbolAST)adaptor.rulePostProcessing(root_0);
            adaptor.setTokenBoundaries(retval.tree, retval.start, retval.stop);
            }
        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
    	retval.tree = (CymbolAST)adaptor.errorNode(input, retval.start, input.LT(-1), re);

        }
        finally {
        }
        return retval;
    }
    // $ANTLR end "expressionList"

    public static class expression_return extends ParserRuleReturnScope {
        CymbolAST tree;
        public Object getTree() { return tree; }
    };

    // $ANTLR start "expression"
    // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Cymbol.g:87:1: expression : addExpression -> ^( EXPR addExpression ) ;
    public final CymbolParser.expression_return expression() throws RecognitionException {
        CymbolParser.expression_return retval = new CymbolParser.expression_return();
        retval.start = input.LT(1);

        CymbolAST root_0 = null;

        CymbolParser.addExpression_return addExpression56 = null;


        RewriteRuleSubtreeStream stream_addExpression=new RewriteRuleSubtreeStream(adaptor,"rule addExpression");
        try {
            // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Cymbol.g:88:5: ( addExpression -> ^( EXPR addExpression ) )
            // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Cymbol.g:88:9: addExpression
            {
            pushFollow(FOLLOW_addExpression_in_expression708);
            addExpression56=addExpression();

            state._fsp--;
            if (state.failed) return retval;
            if ( state.backtracking==0 ) stream_addExpression.add(addExpression56.getTree());


            // AST REWRITE
            // elements: addExpression
            // token labels: 
            // rule labels: retval
            // token list labels: 
            // rule list labels: 
            // wildcard labels: 
            if ( state.backtracking==0 ) {
            retval.tree = root_0;
            RewriteRuleSubtreeStream stream_retval=new RewriteRuleSubtreeStream(adaptor,"rule retval",retval!=null?retval.tree:null);

            root_0 = (CymbolAST)adaptor.nil();
            // 88:23: -> ^( EXPR addExpression )
            {
                // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Cymbol.g:88:26: ^( EXPR addExpression )
                {
                CymbolAST root_1 = (CymbolAST)adaptor.nil();
                root_1 = (CymbolAST)adaptor.becomeRoot((CymbolAST)adaptor.create(EXPR, "EXPR"), root_1);

                adaptor.addChild(root_1, stream_addExpression.nextTree());

                adaptor.addChild(root_0, root_1);
                }

            }

            retval.tree = root_0;}
            }

            retval.stop = input.LT(-1);

            if ( state.backtracking==0 ) {

            retval.tree = (CymbolAST)adaptor.rulePostProcessing(root_0);
            adaptor.setTokenBoundaries(retval.tree, retval.start, retval.stop);
            }
        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
    	retval.tree = (CymbolAST)adaptor.errorNode(input, retval.start, input.LT(-1), re);

        }
        finally {
        }
        return retval;
    }
    // $ANTLR end "expression"

    public static class addExpression_return extends ParserRuleReturnScope {
        CymbolAST tree;
        public Object getTree() { return tree; }
    };

    // $ANTLR start "addExpression"
    // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Cymbol.g:91:1: addExpression : postfixExpression ( '+' postfixExpression )* ;
    public final CymbolParser.addExpression_return addExpression() throws RecognitionException {
        CymbolParser.addExpression_return retval = new CymbolParser.addExpression_return();
        retval.start = input.LT(1);

        CymbolAST root_0 = null;

        Token char_literal58=null;
        CymbolParser.postfixExpression_return postfixExpression57 = null;

        CymbolParser.postfixExpression_return postfixExpression59 = null;


        CymbolAST char_literal58_tree=null;

        try {
            // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Cymbol.g:92:2: ( postfixExpression ( '+' postfixExpression )* )
            // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Cymbol.g:92:4: postfixExpression ( '+' postfixExpression )*
            {
            root_0 = (CymbolAST)adaptor.nil();

            pushFollow(FOLLOW_postfixExpression_in_addExpression734);
            postfixExpression57=postfixExpression();

            state._fsp--;
            if (state.failed) return retval;
            if ( state.backtracking==0 ) adaptor.addChild(root_0, postfixExpression57.getTree());
            // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Cymbol.g:92:22: ( '+' postfixExpression )*
            loop15:
            do {
                int alt15=2;
                int LA15_0 = input.LA(1);

                if ( (LA15_0==33) ) {
                    alt15=1;
                }


                switch (alt15) {
            	case 1 :
            	    // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Cymbol.g:92:23: '+' postfixExpression
            	    {
            	    char_literal58=(Token)match(input,33,FOLLOW_33_in_addExpression737); if (state.failed) return retval;
            	    if ( state.backtracking==0 ) {
            	    char_literal58_tree = (CymbolAST)adaptor.create(char_literal58);
            	    root_0 = (CymbolAST)adaptor.becomeRoot(char_literal58_tree, root_0);
            	    }
            	    pushFollow(FOLLOW_postfixExpression_in_addExpression740);
            	    postfixExpression59=postfixExpression();

            	    state._fsp--;
            	    if (state.failed) return retval;
            	    if ( state.backtracking==0 ) adaptor.addChild(root_0, postfixExpression59.getTree());

            	    }
            	    break;

            	default :
            	    break loop15;
                }
            } while (true);


            }

            retval.stop = input.LT(-1);

            if ( state.backtracking==0 ) {

            retval.tree = (CymbolAST)adaptor.rulePostProcessing(root_0);
            adaptor.setTokenBoundaries(retval.tree, retval.start, retval.stop);
            }
        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
    	retval.tree = (CymbolAST)adaptor.errorNode(input, retval.start, input.LT(-1), re);

        }
        finally {
        }
        return retval;
    }
    // $ANTLR end "addExpression"

    public static class postfixExpression_return extends ParserRuleReturnScope {
        CymbolAST tree;
        public Object getTree() { return tree; }
    };

    // $ANTLR start "postfixExpression"
    // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Cymbol.g:96:1: postfixExpression : ( primary -> primary ) ( options {backtrack=true; } : '.' ID '(' expressionList ')' -> ^( CALL ^( '.' $postfixExpression ID ) ) | '.' ID -> ^( '.' $postfixExpression ID ) | '(' expressionList ')' -> ^( CALL $postfixExpression) )* ;
    public final CymbolParser.postfixExpression_return postfixExpression() throws RecognitionException {
        CymbolParser.postfixExpression_return retval = new CymbolParser.postfixExpression_return();
        retval.start = input.LT(1);

        CymbolAST root_0 = null;

        Token char_literal61=null;
        Token ID62=null;
        Token char_literal63=null;
        Token char_literal65=null;
        Token char_literal66=null;
        Token ID67=null;
        Token char_literal68=null;
        Token char_literal70=null;
        CymbolParser.primary_return primary60 = null;

        CymbolParser.expressionList_return expressionList64 = null;

        CymbolParser.expressionList_return expressionList69 = null;


        CymbolAST char_literal61_tree=null;
        CymbolAST ID62_tree=null;
        CymbolAST char_literal63_tree=null;
        CymbolAST char_literal65_tree=null;
        CymbolAST char_literal66_tree=null;
        CymbolAST ID67_tree=null;
        CymbolAST char_literal68_tree=null;
        CymbolAST char_literal70_tree=null;
        RewriteRuleTokenStream stream_26=new RewriteRuleTokenStream(adaptor,"token 26");
        RewriteRuleTokenStream stream_27=new RewriteRuleTokenStream(adaptor,"token 27");
        RewriteRuleTokenStream stream_ID=new RewriteRuleTokenStream(adaptor,"token ID");
        RewriteRuleTokenStream stream_34=new RewriteRuleTokenStream(adaptor,"token 34");
        RewriteRuleSubtreeStream stream_primary=new RewriteRuleSubtreeStream(adaptor,"rule primary");
        RewriteRuleSubtreeStream stream_expressionList=new RewriteRuleSubtreeStream(adaptor,"rule expressionList");
        try {
            // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Cymbol.g:97:5: ( ( primary -> primary ) ( options {backtrack=true; } : '.' ID '(' expressionList ')' -> ^( CALL ^( '.' $postfixExpression ID ) ) | '.' ID -> ^( '.' $postfixExpression ID ) | '(' expressionList ')' -> ^( CALL $postfixExpression) )* )
            // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Cymbol.g:97:9: ( primary -> primary ) ( options {backtrack=true; } : '.' ID '(' expressionList ')' -> ^( CALL ^( '.' $postfixExpression ID ) ) | '.' ID -> ^( '.' $postfixExpression ID ) | '(' expressionList ')' -> ^( CALL $postfixExpression) )*
            {
            // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Cymbol.g:97:9: ( primary -> primary )
            // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Cymbol.g:97:10: primary
            {
            pushFollow(FOLLOW_primary_in_postfixExpression760);
            primary60=primary();

            state._fsp--;
            if (state.failed) return retval;
            if ( state.backtracking==0 ) stream_primary.add(primary60.getTree());


            // AST REWRITE
            // elements: primary
            // token labels: 
            // rule labels: retval
            // token list labels: 
            // rule list labels: 
            // wildcard labels: 
            if ( state.backtracking==0 ) {
            retval.tree = root_0;
            RewriteRuleSubtreeStream stream_retval=new RewriteRuleSubtreeStream(adaptor,"rule retval",retval!=null?retval.tree:null);

            root_0 = (CymbolAST)adaptor.nil();
            // 97:17: -> primary
            {
                adaptor.addChild(root_0, stream_primary.nextTree());

            }

            retval.tree = root_0;}
            }

            // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Cymbol.g:98:6: ( options {backtrack=true; } : '.' ID '(' expressionList ')' -> ^( CALL ^( '.' $postfixExpression ID ) ) | '.' ID -> ^( '.' $postfixExpression ID ) | '(' expressionList ')' -> ^( CALL $postfixExpression) )*
            loop16:
            do {
                int alt16=4;
                alt16 = dfa16.predict(input);
                switch (alt16) {
            	case 1 :
            	    // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Cymbol.g:99:5: '.' ID '(' expressionList ')'
            	    {
            	    char_literal61=(Token)match(input,34,FOLLOW_34_in_postfixExpression783); if (state.failed) return retval; 
            	    if ( state.backtracking==0 ) stream_34.add(char_literal61);

            	    ID62=(Token)match(input,ID,FOLLOW_ID_in_postfixExpression785); if (state.failed) return retval; 
            	    if ( state.backtracking==0 ) stream_ID.add(ID62);

            	    char_literal63=(Token)match(input,26,FOLLOW_26_in_postfixExpression787); if (state.failed) return retval; 
            	    if ( state.backtracking==0 ) stream_26.add(char_literal63);

            	    pushFollow(FOLLOW_expressionList_in_postfixExpression789);
            	    expressionList64=expressionList();

            	    state._fsp--;
            	    if (state.failed) return retval;
            	    if ( state.backtracking==0 ) stream_expressionList.add(expressionList64.getTree());
            	    char_literal65=(Token)match(input,27,FOLLOW_27_in_postfixExpression791); if (state.failed) return retval; 
            	    if ( state.backtracking==0 ) stream_27.add(char_literal65);



            	    // AST REWRITE
            	    // elements: ID, postfixExpression, 34
            	    // token labels: 
            	    // rule labels: retval
            	    // token list labels: 
            	    // rule list labels: 
            	    // wildcard labels: 
            	    if ( state.backtracking==0 ) {
            	    retval.tree = root_0;
            	    RewriteRuleSubtreeStream stream_retval=new RewriteRuleSubtreeStream(adaptor,"rule retval",retval!=null?retval.tree:null);

            	    root_0 = (CymbolAST)adaptor.nil();
            	    // 99:35: -> ^( CALL ^( '.' $postfixExpression ID ) )
            	    {
            	        // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Cymbol.g:99:38: ^( CALL ^( '.' $postfixExpression ID ) )
            	        {
            	        CymbolAST root_1 = (CymbolAST)adaptor.nil();
            	        root_1 = (CymbolAST)adaptor.becomeRoot((CymbolAST)adaptor.create(CALL, "CALL"), root_1);

            	        // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Cymbol.g:99:45: ^( '.' $postfixExpression ID )
            	        {
            	        CymbolAST root_2 = (CymbolAST)adaptor.nil();
            	        root_2 = (CymbolAST)adaptor.becomeRoot(stream_34.nextNode(), root_2);

            	        adaptor.addChild(root_2, stream_retval.nextTree());
            	        adaptor.addChild(root_2, stream_ID.nextNode());

            	        adaptor.addChild(root_1, root_2);
            	        }

            	        adaptor.addChild(root_0, root_1);
            	        }

            	    }

            	    retval.tree = root_0;}
            	    }
            	    break;
            	case 2 :
            	    // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Cymbol.g:100:5: '.' ID
            	    {
            	    char_literal66=(Token)match(input,34,FOLLOW_34_in_postfixExpression812); if (state.failed) return retval; 
            	    if ( state.backtracking==0 ) stream_34.add(char_literal66);

            	    ID67=(Token)match(input,ID,FOLLOW_ID_in_postfixExpression814); if (state.failed) return retval; 
            	    if ( state.backtracking==0 ) stream_ID.add(ID67);



            	    // AST REWRITE
            	    // elements: ID, postfixExpression, 34
            	    // token labels: 
            	    // rule labels: retval
            	    // token list labels: 
            	    // rule list labels: 
            	    // wildcard labels: 
            	    if ( state.backtracking==0 ) {
            	    retval.tree = root_0;
            	    RewriteRuleSubtreeStream stream_retval=new RewriteRuleSubtreeStream(adaptor,"rule retval",retval!=null?retval.tree:null);

            	    root_0 = (CymbolAST)adaptor.nil();
            	    // 100:19: -> ^( '.' $postfixExpression ID )
            	    {
            	        // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Cymbol.g:100:22: ^( '.' $postfixExpression ID )
            	        {
            	        CymbolAST root_1 = (CymbolAST)adaptor.nil();
            	        root_1 = (CymbolAST)adaptor.becomeRoot(stream_34.nextNode(), root_1);

            	        adaptor.addChild(root_1, stream_retval.nextTree());
            	        adaptor.addChild(root_1, stream_ID.nextNode());

            	        adaptor.addChild(root_0, root_1);
            	        }

            	    }

            	    retval.tree = root_0;}
            	    }
            	    break;
            	case 3 :
            	    // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Cymbol.g:101:5: '(' expressionList ')'
            	    {
            	    char_literal68=(Token)match(input,26,FOLLOW_26_in_postfixExpression838); if (state.failed) return retval; 
            	    if ( state.backtracking==0 ) stream_26.add(char_literal68);

            	    pushFollow(FOLLOW_expressionList_in_postfixExpression840);
            	    expressionList69=expressionList();

            	    state._fsp--;
            	    if (state.failed) return retval;
            	    if ( state.backtracking==0 ) stream_expressionList.add(expressionList69.getTree());
            	    char_literal70=(Token)match(input,27,FOLLOW_27_in_postfixExpression842); if (state.failed) return retval; 
            	    if ( state.backtracking==0 ) stream_27.add(char_literal70);



            	    // AST REWRITE
            	    // elements: postfixExpression
            	    // token labels: 
            	    // rule labels: retval
            	    // token list labels: 
            	    // rule list labels: 
            	    // wildcard labels: 
            	    if ( state.backtracking==0 ) {
            	    retval.tree = root_0;
            	    RewriteRuleSubtreeStream stream_retval=new RewriteRuleSubtreeStream(adaptor,"rule retval",retval!=null?retval.tree:null);

            	    root_0 = (CymbolAST)adaptor.nil();
            	    // 101:35: -> ^( CALL $postfixExpression)
            	    {
            	        // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Cymbol.g:101:38: ^( CALL $postfixExpression)
            	        {
            	        CymbolAST root_1 = (CymbolAST)adaptor.nil();
            	        root_1 = (CymbolAST)adaptor.becomeRoot((CymbolAST)adaptor.create(CALL, "CALL"), root_1);

            	        adaptor.addChild(root_1, stream_retval.nextTree());

            	        adaptor.addChild(root_0, root_1);
            	        }

            	    }

            	    retval.tree = root_0;}
            	    }
            	    break;

            	default :
            	    break loop16;
                }
            } while (true);


            }

            retval.stop = input.LT(-1);

            if ( state.backtracking==0 ) {

            retval.tree = (CymbolAST)adaptor.rulePostProcessing(root_0);
            adaptor.setTokenBoundaries(retval.tree, retval.start, retval.stop);
            }
        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
    	retval.tree = (CymbolAST)adaptor.errorNode(input, retval.start, input.LT(-1), re);

        }
        finally {
        }
        return retval;
    }
    // $ANTLR end "postfixExpression"

    public static class suffix_return extends ParserRuleReturnScope {
        CymbolAST tree;
        public Object getTree() { return tree; }
    };

    // $ANTLR start "suffix"
    // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Cymbol.g:106:1: suffix[CommonTree expr] options {backtrack=true; } : ( '.' ID '(' expressionList ')' -> ^( CALL ^( '.' ID ) ) | '.' ID -> ^( '.' ID ) | '(' expressionList ')' -> ^( CALL ) );
    public final CymbolParser.suffix_return suffix(CommonTree expr) throws RecognitionException {
        CymbolParser.suffix_return retval = new CymbolParser.suffix_return();
        retval.start = input.LT(1);

        CymbolAST root_0 = null;

        Token char_literal71=null;
        Token ID72=null;
        Token char_literal73=null;
        Token char_literal75=null;
        Token char_literal76=null;
        Token ID77=null;
        Token char_literal78=null;
        Token char_literal80=null;
        CymbolParser.expressionList_return expressionList74 = null;

        CymbolParser.expressionList_return expressionList79 = null;


        CymbolAST char_literal71_tree=null;
        CymbolAST ID72_tree=null;
        CymbolAST char_literal73_tree=null;
        CymbolAST char_literal75_tree=null;
        CymbolAST char_literal76_tree=null;
        CymbolAST ID77_tree=null;
        CymbolAST char_literal78_tree=null;
        CymbolAST char_literal80_tree=null;
        RewriteRuleTokenStream stream_26=new RewriteRuleTokenStream(adaptor,"token 26");
        RewriteRuleTokenStream stream_27=new RewriteRuleTokenStream(adaptor,"token 27");
        RewriteRuleTokenStream stream_ID=new RewriteRuleTokenStream(adaptor,"token ID");
        RewriteRuleTokenStream stream_34=new RewriteRuleTokenStream(adaptor,"token 34");
        RewriteRuleSubtreeStream stream_expressionList=new RewriteRuleSubtreeStream(adaptor,"rule expressionList");
        try {
            // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Cymbol.g:108:2: ( '.' ID '(' expressionList ')' -> ^( CALL ^( '.' ID ) ) | '.' ID -> ^( '.' ID ) | '(' expressionList ')' -> ^( CALL ) )
            int alt17=3;
            int LA17_0 = input.LA(1);

            if ( (LA17_0==34) ) {
                int LA17_1 = input.LA(2);

                if ( (LA17_1==ID) ) {
                    int LA17_3 = input.LA(3);

                    if ( (LA17_3==26) ) {
                        alt17=1;
                    }
                    else if ( (LA17_3==EOF) ) {
                        alt17=2;
                    }
                    else {
                        if (state.backtracking>0) {state.failed=true; return retval;}
                        NoViableAltException nvae =
                            new NoViableAltException("", 17, 3, input);

                        throw nvae;
                    }
                }
                else {
                    if (state.backtracking>0) {state.failed=true; return retval;}
                    NoViableAltException nvae =
                        new NoViableAltException("", 17, 1, input);

                    throw nvae;
                }
            }
            else if ( (LA17_0==26) ) {
                alt17=3;
            }
            else {
                if (state.backtracking>0) {state.failed=true; return retval;}
                NoViableAltException nvae =
                    new NoViableAltException("", 17, 0, input);

                throw nvae;
            }
            switch (alt17) {
                case 1 :
                    // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Cymbol.g:108:4: '.' ID '(' expressionList ')'
                    {
                    char_literal71=(Token)match(input,34,FOLLOW_34_in_suffix886); if (state.failed) return retval; 
                    if ( state.backtracking==0 ) stream_34.add(char_literal71);

                    ID72=(Token)match(input,ID,FOLLOW_ID_in_suffix888); if (state.failed) return retval; 
                    if ( state.backtracking==0 ) stream_ID.add(ID72);

                    char_literal73=(Token)match(input,26,FOLLOW_26_in_suffix890); if (state.failed) return retval; 
                    if ( state.backtracking==0 ) stream_26.add(char_literal73);

                    pushFollow(FOLLOW_expressionList_in_suffix892);
                    expressionList74=expressionList();

                    state._fsp--;
                    if (state.failed) return retval;
                    if ( state.backtracking==0 ) stream_expressionList.add(expressionList74.getTree());
                    char_literal75=(Token)match(input,27,FOLLOW_27_in_suffix894); if (state.failed) return retval; 
                    if ( state.backtracking==0 ) stream_27.add(char_literal75);



                    // AST REWRITE
                    // elements: 34, ID
                    // token labels: 
                    // rule labels: retval
                    // token list labels: 
                    // rule list labels: 
                    // wildcard labels: 
                    if ( state.backtracking==0 ) {
                    retval.tree = root_0;
                    RewriteRuleSubtreeStream stream_retval=new RewriteRuleSubtreeStream(adaptor,"rule retval",retval!=null?retval.tree:null);

                    root_0 = (CymbolAST)adaptor.nil();
                    // 108:34: -> ^( CALL ^( '.' ID ) )
                    {
                        // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Cymbol.g:108:37: ^( CALL ^( '.' ID ) )
                        {
                        CymbolAST root_1 = (CymbolAST)adaptor.nil();
                        root_1 = (CymbolAST)adaptor.becomeRoot((CymbolAST)adaptor.create(CALL, "CALL"), root_1);

                        // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Cymbol.g:108:44: ^( '.' ID )
                        {
                        CymbolAST root_2 = (CymbolAST)adaptor.nil();
                        root_2 = (CymbolAST)adaptor.becomeRoot(stream_34.nextNode(), root_2);

                        adaptor.addChild(root_2, expr);
                        adaptor.addChild(root_2, stream_ID.nextNode());

                        adaptor.addChild(root_1, root_2);
                        }

                        adaptor.addChild(root_0, root_1);
                        }

                    }

                    retval.tree = root_0;}
                    }
                    break;
                case 2 :
                    // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Cymbol.g:109:4: '.' ID
                    {
                    char_literal76=(Token)match(input,34,FOLLOW_34_in_suffix913); if (state.failed) return retval; 
                    if ( state.backtracking==0 ) stream_34.add(char_literal76);

                    ID77=(Token)match(input,ID,FOLLOW_ID_in_suffix915); if (state.failed) return retval; 
                    if ( state.backtracking==0 ) stream_ID.add(ID77);



                    // AST REWRITE
                    // elements: ID, 34
                    // token labels: 
                    // rule labels: retval
                    // token list labels: 
                    // rule list labels: 
                    // wildcard labels: 
                    if ( state.backtracking==0 ) {
                    retval.tree = root_0;
                    RewriteRuleSubtreeStream stream_retval=new RewriteRuleSubtreeStream(adaptor,"rule retval",retval!=null?retval.tree:null);

                    root_0 = (CymbolAST)adaptor.nil();
                    // 109:18: -> ^( '.' ID )
                    {
                        // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Cymbol.g:109:21: ^( '.' ID )
                        {
                        CymbolAST root_1 = (CymbolAST)adaptor.nil();
                        root_1 = (CymbolAST)adaptor.becomeRoot(stream_34.nextNode(), root_1);

                        adaptor.addChild(root_1, expr);
                        adaptor.addChild(root_1, stream_ID.nextNode());

                        adaptor.addChild(root_0, root_1);
                        }

                    }

                    retval.tree = root_0;}
                    }
                    break;
                case 3 :
                    // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Cymbol.g:110:4: '(' expressionList ')'
                    {
                    char_literal78=(Token)match(input,26,FOLLOW_26_in_suffix937); if (state.failed) return retval; 
                    if ( state.backtracking==0 ) stream_26.add(char_literal78);

                    pushFollow(FOLLOW_expressionList_in_suffix939);
                    expressionList79=expressionList();

                    state._fsp--;
                    if (state.failed) return retval;
                    if ( state.backtracking==0 ) stream_expressionList.add(expressionList79.getTree());
                    char_literal80=(Token)match(input,27,FOLLOW_27_in_suffix941); if (state.failed) return retval; 
                    if ( state.backtracking==0 ) stream_27.add(char_literal80);



                    // AST REWRITE
                    // elements: 
                    // token labels: 
                    // rule labels: retval
                    // token list labels: 
                    // rule list labels: 
                    // wildcard labels: 
                    if ( state.backtracking==0 ) {
                    retval.tree = root_0;
                    RewriteRuleSubtreeStream stream_retval=new RewriteRuleSubtreeStream(adaptor,"rule retval",retval!=null?retval.tree:null);

                    root_0 = (CymbolAST)adaptor.nil();
                    // 110:34: -> ^( CALL )
                    {
                        // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Cymbol.g:110:37: ^( CALL )
                        {
                        CymbolAST root_1 = (CymbolAST)adaptor.nil();
                        root_1 = (CymbolAST)adaptor.becomeRoot((CymbolAST)adaptor.create(CALL, "CALL"), root_1);

                        adaptor.addChild(root_1, expr);

                        adaptor.addChild(root_0, root_1);
                        }

                    }

                    retval.tree = root_0;}
                    }
                    break;

            }
            retval.stop = input.LT(-1);

            if ( state.backtracking==0 ) {

            retval.tree = (CymbolAST)adaptor.rulePostProcessing(root_0);
            adaptor.setTokenBoundaries(retval.tree, retval.start, retval.stop);
            }
        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
    	retval.tree = (CymbolAST)adaptor.errorNode(input, retval.start, input.LT(-1), re);

        }
        finally {
        }
        return retval;
    }
    // $ANTLR end "suffix"

    public static class primary_return extends ParserRuleReturnScope {
        CymbolAST tree;
        public Object getTree() { return tree; }
    };

    // $ANTLR start "primary"
    // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Cymbol.g:113:1: primary : ( 'this' | 'super' | ID | INT | '(' expression ')' -> expression );
    public final CymbolParser.primary_return primary() throws RecognitionException {
        CymbolParser.primary_return retval = new CymbolParser.primary_return();
        retval.start = input.LT(1);

        CymbolAST root_0 = null;

        Token string_literal81=null;
        Token string_literal82=null;
        Token ID83=null;
        Token INT84=null;
        Token char_literal85=null;
        Token char_literal87=null;
        CymbolParser.expression_return expression86 = null;


        CymbolAST string_literal81_tree=null;
        CymbolAST string_literal82_tree=null;
        CymbolAST ID83_tree=null;
        CymbolAST INT84_tree=null;
        CymbolAST char_literal85_tree=null;
        CymbolAST char_literal87_tree=null;
        RewriteRuleTokenStream stream_26=new RewriteRuleTokenStream(adaptor,"token 26");
        RewriteRuleTokenStream stream_27=new RewriteRuleTokenStream(adaptor,"token 27");
        RewriteRuleSubtreeStream stream_expression=new RewriteRuleSubtreeStream(adaptor,"rule expression");
        try {
            // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Cymbol.g:114:5: ( 'this' | 'super' | ID | INT | '(' expression ')' -> expression )
            int alt18=5;
            switch ( input.LA(1) ) {
            case 35:
                {
                alt18=1;
                }
                break;
            case 36:
                {
                alt18=2;
                }
                break;
            case ID:
                {
                alt18=3;
                }
                break;
            case INT:
                {
                alt18=4;
                }
                break;
            case 26:
                {
                alt18=5;
                }
                break;
            default:
                if (state.backtracking>0) {state.failed=true; return retval;}
                NoViableAltException nvae =
                    new NoViableAltException("", 18, 0, input);

                throw nvae;
            }

            switch (alt18) {
                case 1 :
                    // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Cymbol.g:114:9: 'this'
                    {
                    root_0 = (CymbolAST)adaptor.nil();

                    string_literal81=(Token)match(input,35,FOLLOW_35_in_primary973); if (state.failed) return retval;
                    if ( state.backtracking==0 ) {
                    string_literal81_tree = (CymbolAST)adaptor.create(string_literal81);
                    adaptor.addChild(root_0, string_literal81_tree);
                    }

                    }
                    break;
                case 2 :
                    // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Cymbol.g:115:7: 'super'
                    {
                    root_0 = (CymbolAST)adaptor.nil();

                    string_literal82=(Token)match(input,36,FOLLOW_36_in_primary981); if (state.failed) return retval;
                    if ( state.backtracking==0 ) {
                    string_literal82_tree = (CymbolAST)adaptor.create(string_literal82);
                    adaptor.addChild(root_0, string_literal82_tree);
                    }

                    }
                    break;
                case 3 :
                    // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Cymbol.g:116:7: ID
                    {
                    root_0 = (CymbolAST)adaptor.nil();

                    ID83=(Token)match(input,ID,FOLLOW_ID_in_primary989); if (state.failed) return retval;
                    if ( state.backtracking==0 ) {
                    ID83_tree = (CymbolAST)adaptor.create(ID83);
                    adaptor.addChild(root_0, ID83_tree);
                    }

                    }
                    break;
                case 4 :
                    // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Cymbol.g:117:9: INT
                    {
                    root_0 = (CymbolAST)adaptor.nil();

                    INT84=(Token)match(input,INT,FOLLOW_INT_in_primary999); if (state.failed) return retval;
                    if ( state.backtracking==0 ) {
                    INT84_tree = (CymbolAST)adaptor.create(INT84);
                    adaptor.addChild(root_0, INT84_tree);
                    }

                    }
                    break;
                case 5 :
                    // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Cymbol.g:118:9: '(' expression ')'
                    {
                    char_literal85=(Token)match(input,26,FOLLOW_26_in_primary1009); if (state.failed) return retval; 
                    if ( state.backtracking==0 ) stream_26.add(char_literal85);

                    pushFollow(FOLLOW_expression_in_primary1011);
                    expression86=expression();

                    state._fsp--;
                    if (state.failed) return retval;
                    if ( state.backtracking==0 ) stream_expression.add(expression86.getTree());
                    char_literal87=(Token)match(input,27,FOLLOW_27_in_primary1013); if (state.failed) return retval; 
                    if ( state.backtracking==0 ) stream_27.add(char_literal87);



                    // AST REWRITE
                    // elements: expression
                    // token labels: 
                    // rule labels: retval
                    // token list labels: 
                    // rule list labels: 
                    // wildcard labels: 
                    if ( state.backtracking==0 ) {
                    retval.tree = root_0;
                    RewriteRuleSubtreeStream stream_retval=new RewriteRuleSubtreeStream(adaptor,"rule retval",retval!=null?retval.tree:null);

                    root_0 = (CymbolAST)adaptor.nil();
                    // 118:28: -> expression
                    {
                        adaptor.addChild(root_0, stream_expression.nextTree());

                    }

                    retval.tree = root_0;}
                    }
                    break;

            }
            retval.stop = input.LT(-1);

            if ( state.backtracking==0 ) {

            retval.tree = (CymbolAST)adaptor.rulePostProcessing(root_0);
            adaptor.setTokenBoundaries(retval.tree, retval.start, retval.stop);
            }
        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
    	retval.tree = (CymbolAST)adaptor.errorNode(input, retval.start, input.LT(-1), re);

        }
        finally {
        }
        return retval;
    }
    // $ANTLR end "primary"

    // $ANTLR start synpred1_Cymbol
    public final void synpred1_Cymbol_fragment() throws RecognitionException {   
        // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Cymbol.g:99:5: ( '.' ID '(' expressionList ')' )
        // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Cymbol.g:99:5: '.' ID '(' expressionList ')'
        {
        match(input,34,FOLLOW_34_in_synpred1_Cymbol783); if (state.failed) return ;
        match(input,ID,FOLLOW_ID_in_synpred1_Cymbol785); if (state.failed) return ;
        match(input,26,FOLLOW_26_in_synpred1_Cymbol787); if (state.failed) return ;
        pushFollow(FOLLOW_expressionList_in_synpred1_Cymbol789);
        expressionList();

        state._fsp--;
        if (state.failed) return ;
        match(input,27,FOLLOW_27_in_synpred1_Cymbol791); if (state.failed) return ;

        }
    }
    // $ANTLR end synpred1_Cymbol

    // $ANTLR start synpred2_Cymbol
    public final void synpred2_Cymbol_fragment() throws RecognitionException {   
        // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Cymbol.g:100:5: ( '.' ID )
        // /Users/parrt/research/book/TPDSL/Book/code/symtab/class/Cymbol.g:100:5: '.' ID
        {
        match(input,34,FOLLOW_34_in_synpred2_Cymbol812); if (state.failed) return ;
        match(input,ID,FOLLOW_ID_in_synpred2_Cymbol814); if (state.failed) return ;

        }
    }
    // $ANTLR end synpred2_Cymbol

    // Delegated rules

    public final boolean synpred2_Cymbol() {
        state.backtracking++;
        int start = input.mark();
        try {
            synpred2_Cymbol_fragment(); // can never throw exception
        } catch (RecognitionException re) {
            System.err.println("impossible: "+re);
        }
        boolean success = !state.failed;
        input.rewind(start);
        state.backtracking--;
        state.failed=false;
        return success;
    }
    public final boolean synpred1_Cymbol() {
        state.backtracking++;
        int start = input.mark();
        try {
            synpred1_Cymbol_fragment(); // can never throw exception
        } catch (RecognitionException re) {
            System.err.println("impossible: "+re);
        }
        boolean success = !state.failed;
        input.rewind(start);
        state.backtracking--;
        state.failed=false;
        return success;
    }


    protected DFA16 dfa16 = new DFA16(this);
    static final String DFA16_eotS =
        "\12\uffff";
    static final String DFA16_eofS =
        "\12\uffff";
    static final String DFA16_minS =
        "\1\15\5\uffff\1\0\3\uffff";
    static final String DFA16_maxS =
        "\1\42\5\uffff\1\0\3\uffff";
    static final String DFA16_acceptS =
        "\1\uffff\1\4\5\uffff\1\3\1\1\1\2";
    static final String DFA16_specialS =
        "\6\uffff\1\0\3\uffff}>";
    static final String[] DFA16_transitionS = {
            "\1\1\11\uffff\1\1\2\uffff\1\7\2\1\4\uffff\1\1\1\6",
            "",
            "",
            "",
            "",
            "",
            "\1\uffff",
            "",
            "",
            ""
    };

    static final short[] DFA16_eot = DFA.unpackEncodedString(DFA16_eotS);
    static final short[] DFA16_eof = DFA.unpackEncodedString(DFA16_eofS);
    static final char[] DFA16_min = DFA.unpackEncodedStringToUnsignedChars(DFA16_minS);
    static final char[] DFA16_max = DFA.unpackEncodedStringToUnsignedChars(DFA16_maxS);
    static final short[] DFA16_accept = DFA.unpackEncodedString(DFA16_acceptS);
    static final short[] DFA16_special = DFA.unpackEncodedString(DFA16_specialS);
    static final short[][] DFA16_transition;

    static {
        int numStates = DFA16_transitionS.length;
        DFA16_transition = new short[numStates][];
        for (int i=0; i<numStates; i++) {
            DFA16_transition[i] = DFA.unpackEncodedString(DFA16_transitionS[i]);
        }
    }

    class DFA16 extends DFA {

        public DFA16(BaseRecognizer recognizer) {
            this.recognizer = recognizer;
            this.decisionNumber = 16;
            this.eot = DFA16_eot;
            this.eof = DFA16_eof;
            this.min = DFA16_min;
            this.max = DFA16_max;
            this.accept = DFA16_accept;
            this.special = DFA16_special;
            this.transition = DFA16_transition;
        }
        public String getDescription() {
            return "()* loopback of 98:6: ( options {backtrack=true; } : '.' ID '(' expressionList ')' -> ^( CALL ^( '.' $postfixExpression ID ) ) | '.' ID -> ^( '.' $postfixExpression ID ) | '(' expressionList ')' -> ^( CALL $postfixExpression) )*";
        }
        public int specialStateTransition(int s, IntStream _input) throws NoViableAltException {
            TokenStream input = (TokenStream)_input;
        	int _s = s;
            switch ( s ) {
                    case 0 : 
                        int LA16_6 = input.LA(1);

                         
                        int index16_6 = input.index();
                        input.rewind();
                        s = -1;
                        if ( (synpred1_Cymbol()) ) {s = 8;}

                        else if ( (synpred2_Cymbol()) ) {s = 9;}

                         
                        input.seek(index16_6);
                        if ( s>=0 ) return s;
                        break;
            }
            if (state.backtracking>0) {state.failed=true; return -1;}
            NoViableAltException nvae =
                new NoViableAltException(getDescription(), 16, _s, input);
            error(nvae);
            throw nvae;
        }
    }
 

    public static final BitSet FOLLOW_classDefinition_in_compilationUnit138 = new BitSet(new long[]{0x00000000E0108000L});
    public static final BitSet FOLLOW_varDeclaration_in_compilationUnit142 = new BitSet(new long[]{0x00000000E0108000L});
    public static final BitSet FOLLOW_methodDeclaration_in_compilationUnit146 = new BitSet(new long[]{0x00000000E0108000L});
    public static final BitSet FOLLOW_EOF_in_compilationUnit151 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_20_in_classDefinition171 = new BitSet(new long[]{0x0000000000008000L});
    public static final BitSet FOLLOW_ID_in_classDefinition173 = new BitSet(new long[]{0x0000000001200000L});
    public static final BitSet FOLLOW_superClass_in_classDefinition175 = new BitSet(new long[]{0x0000000000200000L});
    public static final BitSet FOLLOW_21_in_classDefinition178 = new BitSet(new long[]{0x00000000E2108000L});
    public static final BitSet FOLLOW_classMember_in_classDefinition180 = new BitSet(new long[]{0x00000000E2508000L});
    public static final BitSet FOLLOW_22_in_classDefinition183 = new BitSet(new long[]{0x0000000000800000L});
    public static final BitSet FOLLOW_23_in_classDefinition185 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_24_in_superClass224 = new BitSet(new long[]{0x0000000002000000L});
    public static final BitSet FOLLOW_25_in_superClass226 = new BitSet(new long[]{0x0000000000008000L});
    public static final BitSet FOLLOW_ID_in_superClass228 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_type_in_classMember248 = new BitSet(new long[]{0x0000000000008000L});
    public static final BitSet FOLLOW_ID_in_classMember250 = new BitSet(new long[]{0x0000000000802000L});
    public static final BitSet FOLLOW_ASSIGN_in_classMember253 = new BitSet(new long[]{0x0000001804018000L});
    public static final BitSet FOLLOW_expression_in_classMember255 = new BitSet(new long[]{0x0000000000800000L});
    public static final BitSet FOLLOW_23_in_classMember259 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_methodDeclaration_in_classMember277 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_25_in_classMember282 = new BitSet(new long[]{0x0000000001000000L});
    public static final BitSet FOLLOW_24_in_classMember284 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_type_in_methodDeclaration305 = new BitSet(new long[]{0x0000000000008000L});
    public static final BitSet FOLLOW_ID_in_methodDeclaration307 = new BitSet(new long[]{0x0000000004000000L});
    public static final BitSet FOLLOW_26_in_methodDeclaration309 = new BitSet(new long[]{0x00000000E8008000L});
    public static final BitSet FOLLOW_formalParameters_in_methodDeclaration311 = new BitSet(new long[]{0x0000000008000000L});
    public static final BitSet FOLLOW_27_in_methodDeclaration314 = new BitSet(new long[]{0x0000000000200000L});
    public static final BitSet FOLLOW_block_in_methodDeclaration316 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_type_in_formalParameters359 = new BitSet(new long[]{0x0000000000008000L});
    public static final BitSet FOLLOW_ID_in_formalParameters361 = new BitSet(new long[]{0x0000000010000002L});
    public static final BitSet FOLLOW_28_in_formalParameters364 = new BitSet(new long[]{0x00000000E0008000L});
    public static final BitSet FOLLOW_type_in_formalParameters366 = new BitSet(new long[]{0x0000000000008000L});
    public static final BitSet FOLLOW_ID_in_formalParameters368 = new BitSet(new long[]{0x0000000010000002L});
    public static final BitSet FOLLOW_set_in_type0 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_21_in_block442 = new BitSet(new long[]{0x00000019E4E18000L});
    public static final BitSet FOLLOW_statement_in_block444 = new BitSet(new long[]{0x00000019E4E18000L});
    public static final BitSet FOLLOW_22_in_block447 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_type_in_varDeclaration477 = new BitSet(new long[]{0x0000000000008000L});
    public static final BitSet FOLLOW_ID_in_varDeclaration479 = new BitSet(new long[]{0x0000000000802000L});
    public static final BitSet FOLLOW_ASSIGN_in_varDeclaration482 = new BitSet(new long[]{0x0000001804018000L});
    public static final BitSet FOLLOW_expression_in_varDeclaration484 = new BitSet(new long[]{0x0000000000800000L});
    public static final BitSet FOLLOW_23_in_varDeclaration488 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_block_in_statement521 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_varDeclaration_in_statement529 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_32_in_statement539 = new BitSet(new long[]{0x0000001804818000L});
    public static final BitSet FOLLOW_expression_in_statement541 = new BitSet(new long[]{0x0000000000800000L});
    public static final BitSet FOLLOW_23_in_statement544 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_postfixExpression_in_statement563 = new BitSet(new long[]{0x0000000000802000L});
    public static final BitSet FOLLOW_ASSIGN_in_statement578 = new BitSet(new long[]{0x0000001804018000L});
    public static final BitSet FOLLOW_expression_in_statement580 = new BitSet(new long[]{0x0000000000800000L});
    public static final BitSet FOLLOW_23_in_statement630 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_23_in_statement639 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_expression_in_expressionList661 = new BitSet(new long[]{0x0000000010000002L});
    public static final BitSet FOLLOW_28_in_expressionList664 = new BitSet(new long[]{0x0000001804018000L});
    public static final BitSet FOLLOW_expression_in_expressionList666 = new BitSet(new long[]{0x0000000010000002L});
    public static final BitSet FOLLOW_addExpression_in_expression708 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_postfixExpression_in_addExpression734 = new BitSet(new long[]{0x0000000200000002L});
    public static final BitSet FOLLOW_33_in_addExpression737 = new BitSet(new long[]{0x0000001804018000L});
    public static final BitSet FOLLOW_postfixExpression_in_addExpression740 = new BitSet(new long[]{0x0000000200000002L});
    public static final BitSet FOLLOW_primary_in_postfixExpression760 = new BitSet(new long[]{0x0000000404000002L});
    public static final BitSet FOLLOW_34_in_postfixExpression783 = new BitSet(new long[]{0x0000000000008000L});
    public static final BitSet FOLLOW_ID_in_postfixExpression785 = new BitSet(new long[]{0x0000000004000000L});
    public static final BitSet FOLLOW_26_in_postfixExpression787 = new BitSet(new long[]{0x000000180C018000L});
    public static final BitSet FOLLOW_expressionList_in_postfixExpression789 = new BitSet(new long[]{0x0000000008000000L});
    public static final BitSet FOLLOW_27_in_postfixExpression791 = new BitSet(new long[]{0x0000000404000002L});
    public static final BitSet FOLLOW_34_in_postfixExpression812 = new BitSet(new long[]{0x0000000000008000L});
    public static final BitSet FOLLOW_ID_in_postfixExpression814 = new BitSet(new long[]{0x0000000404000002L});
    public static final BitSet FOLLOW_26_in_postfixExpression838 = new BitSet(new long[]{0x000000180C018000L});
    public static final BitSet FOLLOW_expressionList_in_postfixExpression840 = new BitSet(new long[]{0x0000000008000000L});
    public static final BitSet FOLLOW_27_in_postfixExpression842 = new BitSet(new long[]{0x0000000404000002L});
    public static final BitSet FOLLOW_34_in_suffix886 = new BitSet(new long[]{0x0000000000008000L});
    public static final BitSet FOLLOW_ID_in_suffix888 = new BitSet(new long[]{0x0000000004000000L});
    public static final BitSet FOLLOW_26_in_suffix890 = new BitSet(new long[]{0x000000180C018000L});
    public static final BitSet FOLLOW_expressionList_in_suffix892 = new BitSet(new long[]{0x0000000008000000L});
    public static final BitSet FOLLOW_27_in_suffix894 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_34_in_suffix913 = new BitSet(new long[]{0x0000000000008000L});
    public static final BitSet FOLLOW_ID_in_suffix915 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_26_in_suffix937 = new BitSet(new long[]{0x000000180C018000L});
    public static final BitSet FOLLOW_expressionList_in_suffix939 = new BitSet(new long[]{0x0000000008000000L});
    public static final BitSet FOLLOW_27_in_suffix941 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_35_in_primary973 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_36_in_primary981 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_ID_in_primary989 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_INT_in_primary999 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_26_in_primary1009 = new BitSet(new long[]{0x0000001804018000L});
    public static final BitSet FOLLOW_expression_in_primary1011 = new BitSet(new long[]{0x0000000008000000L});
    public static final BitSet FOLLOW_27_in_primary1013 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_34_in_synpred1_Cymbol783 = new BitSet(new long[]{0x0000000000008000L});
    public static final BitSet FOLLOW_ID_in_synpred1_Cymbol785 = new BitSet(new long[]{0x0000000004000000L});
    public static final BitSet FOLLOW_26_in_synpred1_Cymbol787 = new BitSet(new long[]{0x000000180C018000L});
    public static final BitSet FOLLOW_expressionList_in_synpred1_Cymbol789 = new BitSet(new long[]{0x0000000008000000L});
    public static final BitSet FOLLOW_27_in_synpred1_Cymbol791 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_34_in_synpred2_Cymbol812 = new BitSet(new long[]{0x0000000000008000L});
    public static final BitSet FOLLOW_ID_in_synpred2_Cymbol814 = new BitSet(new long[]{0x0000000000000002L});

}