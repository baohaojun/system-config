// $ANTLR 3.2 debian-7 OrgMode.g 2012-12-20 20:58:48

import org.antlr.runtime.*;
import java.util.Stack;
import java.util.List;
import java.util.ArrayList;
import java.util.Map;
import java.util.HashMap;
import org.antlr.stringtemplate.*;
import org.antlr.stringtemplate.language.*;
import java.util.HashMap;
public class OrgModeParser extends Parser {
    public static final String[] tokenNames = new String[] {
        "<invalid>", "<EOR>", "<DOWN>", "<UP>", "BEGIN_EXAMPLE", "END_EXAMPLE", "BEGIN_SRC", "END_SRC", "STAR", "PLUS", "MINUS", "EQ", "NL", "DOUBLE_NL", "WORD", "WS", "BOLD_INLINE", "UNDERLINED_INLINE", "CODE_INLINE", "VERBATIM_INLINE", "STRIKE_INLINE", "ITALIC_INLINE", "LINK_URL", "LINK_URL_DESC"
    };
    public static final int STAR=8;
    public static final int BEGIN_EXAMPLE=4;
    public static final int DOUBLE_NL=13;
    public static final int LINK_URL=22;
    public static final int STRIKE_INLINE=20;
    public static final int MINUS=10;
    public static final int CODE_INLINE=18;
    public static final int EOF=-1;
    public static final int BOLD_INLINE=16;
    public static final int WORD=14;
    public static final int VERBATIM_INLINE=19;
    public static final int WS=15;
    public static final int UNDERLINED_INLINE=17;
    public static final int END_EXAMPLE=5;
    public static final int END_SRC=7;
    public static final int PLUS=9;
    public static final int LINK_URL_DESC=23;
    public static final int ITALIC_INLINE=21;
    public static final int NL=12;
    public static final int EQ=11;
    public static final int BEGIN_SRC=6;

    // delegates
    // delegators


        public OrgModeParser(TokenStream input) {
            this(input, new RecognizerSharedState());
        }
        public OrgModeParser(TokenStream input, RecognizerSharedState state) {
            super(input, state);
            this.state.ruleMemo = new HashMap[22+1];
             
             
        }
        
    protected StringTemplateGroup templateLib =
      new StringTemplateGroup("OrgModeParserTemplates", AngleBracketTemplateLexer.class);

    public void setTemplateLib(StringTemplateGroup templateLib) {
      this.templateLib = templateLib;
    }
    public StringTemplateGroup getTemplateLib() {
      return templateLib;
    }
    /** allows convenient multi-value initialization:
     *  "new STAttrMap().put(...).put(...)"
     */
    public static class STAttrMap extends HashMap {
      public STAttrMap put(String attrName, Object value) {
        super.put(attrName, value);
        return this;
      }
      public STAttrMap put(String attrName, int value) {
        super.put(attrName, new Integer(value));
        return this;
      }
    }

    public String[] getTokenNames() { return OrgModeParser.tokenNames; }
    public String getGrammarFileName() { return "OrgMode.g"; }


        public boolean notNewlineP() {
            if (input.LT(1) == null) {
                return true;
            }

            int type = input.LT(1).getType();
            System.out.println("NL? " + input.LT(1));
            return ! (type == NL || type == DOUBLE_NL);
        }
        public boolean noSpace() {
            if (input.LT(1) == null) {
                return true;
            }
            boolean result = input.LT(1).getCharPositionInLine() == input.LT(-1).getCharPositionInLine() + input.LT(-1).getText().length();
            System.out.println("noSpace? " + input.LT(1) + " " + input.LT(-1) + ": " + result);
            return result;
        }


    public static class orgFile_return extends ParserRuleReturnScope {
        public StringTemplate st;
        public Object getTemplate() { return st; }
        public String toString() { return st==null?null:st.toString(); }
    };

    // $ANTLR start "orgFile"
    // OrgMode.g:59:1: orgFile : ( block )* ;
    public final OrgModeParser.orgFile_return orgFile() throws RecognitionException {
        OrgModeParser.orgFile_return retval = new OrgModeParser.orgFile_return();
        retval.start = input.LT(1);
        int orgFile_StartIndex = input.index();
        try {
            if ( state.backtracking>0 && alreadyParsedRule(input, 1) ) { return retval; }
            // OrgMode.g:59:9: ( ( block )* )
            // OrgMode.g:59:11: ( block )*
            {
            // OrgMode.g:59:11: ( block )*
            loop1:
            do {
                int alt1=2;
                int LA1_0 = input.LA(1);

                if ( ((LA1_0>=BEGIN_EXAMPLE && LA1_0<=LINK_URL_DESC)) ) {
                    alt1=1;
                }


                switch (alt1) {
            	case 1 :
            	    // OrgMode.g:0:0: block
            	    {
            	    pushFollow(FOLLOW_block_in_orgFile202);
            	    block();

            	    state._fsp--;
            	    if (state.failed) return retval;

            	    }
            	    break;

            	default :
            	    break loop1;
                }
            } while (true);


            }

            retval.stop = input.LT(-1);

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {
            if ( state.backtracking>0 ) { memoize(input, 1, orgFile_StartIndex); }
        }
        return retval;
    }
    // $ANTLR end "orgFile"

    public static class block_return extends ParserRuleReturnScope {
        public StringTemplate st;
        public Object getTemplate() { return st; }
        public String toString() { return st==null?null:st.toString(); }
    };

    // $ANTLR start "block"
    // OrgMode.g:61:1: block : ( header | exampleParagraph | codeParagraph | normalParagraph | NL | DOUBLE_NL );
    public final OrgModeParser.block_return block() throws RecognitionException {
        OrgModeParser.block_return retval = new OrgModeParser.block_return();
        retval.start = input.LT(1);
        int block_StartIndex = input.index();
        try {
            if ( state.backtracking>0 && alreadyParsedRule(input, 2) ) { return retval; }
            // OrgMode.g:61:7: ( header | exampleParagraph | codeParagraph | normalParagraph | NL | DOUBLE_NL )
            int alt2=6;
            alt2 = dfa2.predict(input);
            switch (alt2) {
                case 1 :
                    // OrgMode.g:62:9: header
                    {
                    pushFollow(FOLLOW_header_in_block220);
                    header();

                    state._fsp--;
                    if (state.failed) return retval;

                    }
                    break;
                case 2 :
                    // OrgMode.g:63:9: exampleParagraph
                    {
                    pushFollow(FOLLOW_exampleParagraph_in_block230);
                    exampleParagraph();

                    state._fsp--;
                    if (state.failed) return retval;

                    }
                    break;
                case 3 :
                    // OrgMode.g:64:9: codeParagraph
                    {
                    pushFollow(FOLLOW_codeParagraph_in_block240);
                    codeParagraph();

                    state._fsp--;
                    if (state.failed) return retval;

                    }
                    break;
                case 4 :
                    // OrgMode.g:65:9: normalParagraph
                    {
                    pushFollow(FOLLOW_normalParagraph_in_block250);
                    normalParagraph();

                    state._fsp--;
                    if (state.failed) return retval;

                    }
                    break;
                case 5 :
                    // OrgMode.g:66:9: NL
                    {
                    match(input,NL,FOLLOW_NL_in_block260); if (state.failed) return retval;

                    }
                    break;
                case 6 :
                    // OrgMode.g:67:9: DOUBLE_NL
                    {
                    match(input,DOUBLE_NL,FOLLOW_DOUBLE_NL_in_block270); if (state.failed) return retval;

                    }
                    break;

            }
            retval.stop = input.LT(-1);

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {
            if ( state.backtracking>0 ) { memoize(input, 2, block_StartIndex); }
        }
        return retval;
    }
    // $ANTLR end "block"

    public static class header_return extends ParserRuleReturnScope {
        public StringTemplate st;
        public Object getTemplate() { return st; }
        public String toString() { return st==null?null:st.toString(); }
    };

    // $ANTLR start "header"
    // OrgMode.g:70:1: header : {...}? ( STAR )+ ( notNL )+ ( NL | DOUBLE_NL ) ;
    public final OrgModeParser.header_return header() throws RecognitionException {
        OrgModeParser.header_return retval = new OrgModeParser.header_return();
        retval.start = input.LT(1);
        int header_StartIndex = input.index();
        try {
            if ( state.backtracking>0 && alreadyParsedRule(input, 3) ) { return retval; }
            // OrgMode.g:70:8: ({...}? ( STAR )+ ( notNL )+ ( NL | DOUBLE_NL ) )
            // OrgMode.g:70:10: {...}? ( STAR )+ ( notNL )+ ( NL | DOUBLE_NL )
            {
            if ( !((input.LT(1).getCharPositionInLine() == 0)) ) {
                if (state.backtracking>0) {state.failed=true; return retval;}
                throw new FailedPredicateException(input, "header", "input.LT(1).getCharPositionInLine() == 0");
            }
            // OrgMode.g:70:54: ( STAR )+
            int cnt3=0;
            loop3:
            do {
                int alt3=2;
                int LA3_0 = input.LA(1);

                if ( (LA3_0==STAR) ) {
                    int LA3_1 = input.LA(2);

                    if ( (synpred7_OrgMode()) ) {
                        alt3=1;
                    }


                }


                switch (alt3) {
            	case 1 :
            	    // OrgMode.g:0:0: STAR
            	    {
            	    match(input,STAR,FOLLOW_STAR_in_header285); if (state.failed) return retval;

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

            // OrgMode.g:70:60: ( notNL )+
            int cnt4=0;
            loop4:
            do {
                int alt4=2;
                int LA4_0 = input.LA(1);

                if ( ((LA4_0>=BEGIN_EXAMPLE && LA4_0<=EQ)||(LA4_0>=WORD && LA4_0<=LINK_URL_DESC)) ) {
                    alt4=1;
                }


                switch (alt4) {
            	case 1 :
            	    // OrgMode.g:0:0: notNL
            	    {
            	    pushFollow(FOLLOW_notNL_in_header288);
            	    notNL();

            	    state._fsp--;
            	    if (state.failed) return retval;

            	    }
            	    break;

            	default :
            	    if ( cnt4 >= 1 ) break loop4;
            	    if (state.backtracking>0) {state.failed=true; return retval;}
                        EarlyExitException eee =
                            new EarlyExitException(4, input);
                        throw eee;
                }
                cnt4++;
            } while (true);

            if ( (input.LA(1)>=NL && input.LA(1)<=DOUBLE_NL) ) {
                input.consume();
                state.errorRecovery=false;state.failed=false;
            }
            else {
                if (state.backtracking>0) {state.failed=true; return retval;}
                MismatchedSetException mse = new MismatchedSetException(null,input);
                throw mse;
            }

            if ( state.backtracking==0 ) {
              System.out.println("got a header " + input.toString(retval.start,input.LT(-1)));
            }

            }

            retval.stop = input.LT(-1);

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {
            if ( state.backtracking>0 ) { memoize(input, 3, header_StartIndex); }
        }
        return retval;
    }
    // $ANTLR end "header"

    public static class exampleParagraph_return extends ParserRuleReturnScope {
        public StringTemplate st;
        public Object getTemplate() { return st; }
        public String toString() { return st==null?null:st.toString(); }
    };

    // $ANTLR start "exampleParagraph"
    // OrgMode.g:72:1: exampleParagraph : BEGIN_EXAMPLE normalParagraph END_EXAMPLE ;
    public final OrgModeParser.exampleParagraph_return exampleParagraph() throws RecognitionException {
        OrgModeParser.exampleParagraph_return retval = new OrgModeParser.exampleParagraph_return();
        retval.start = input.LT(1);
        int exampleParagraph_StartIndex = input.index();
        try {
            if ( state.backtracking>0 && alreadyParsedRule(input, 4) ) { return retval; }
            // OrgMode.g:72:18: ( BEGIN_EXAMPLE normalParagraph END_EXAMPLE )
            // OrgMode.g:72:20: BEGIN_EXAMPLE normalParagraph END_EXAMPLE
            {
            match(input,BEGIN_EXAMPLE,FOLLOW_BEGIN_EXAMPLE_in_exampleParagraph305); if (state.failed) return retval;
            pushFollow(FOLLOW_normalParagraph_in_exampleParagraph307);
            normalParagraph();

            state._fsp--;
            if (state.failed) return retval;
            match(input,END_EXAMPLE,FOLLOW_END_EXAMPLE_in_exampleParagraph309); if (state.failed) return retval;

            }

            retval.stop = input.LT(-1);

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {
            if ( state.backtracking>0 ) { memoize(input, 4, exampleParagraph_StartIndex); }
        }
        return retval;
    }
    // $ANTLR end "exampleParagraph"

    public static class codeParagraph_return extends ParserRuleReturnScope {
        public StringTemplate st;
        public Object getTemplate() { return st; }
        public String toString() { return st==null?null:st.toString(); }
    };

    // $ANTLR start "codeParagraph"
    // OrgMode.g:74:1: codeParagraph : BEGIN_SRC WORD normalParagraph END_SRC ;
    public final OrgModeParser.codeParagraph_return codeParagraph() throws RecognitionException {
        OrgModeParser.codeParagraph_return retval = new OrgModeParser.codeParagraph_return();
        retval.start = input.LT(1);
        int codeParagraph_StartIndex = input.index();
        try {
            if ( state.backtracking>0 && alreadyParsedRule(input, 5) ) { return retval; }
            // OrgMode.g:74:15: ( BEGIN_SRC WORD normalParagraph END_SRC )
            // OrgMode.g:74:17: BEGIN_SRC WORD normalParagraph END_SRC
            {
            match(input,BEGIN_SRC,FOLLOW_BEGIN_SRC_in_codeParagraph318); if (state.failed) return retval;
            match(input,WORD,FOLLOW_WORD_in_codeParagraph320); if (state.failed) return retval;
            pushFollow(FOLLOW_normalParagraph_in_codeParagraph322);
            normalParagraph();

            state._fsp--;
            if (state.failed) return retval;
            match(input,END_SRC,FOLLOW_END_SRC_in_codeParagraph324); if (state.failed) return retval;

            }

            retval.stop = input.LT(-1);

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {
            if ( state.backtracking>0 ) { memoize(input, 5, codeParagraph_StartIndex); }
        }
        return retval;
    }
    // $ANTLR end "codeParagraph"

    public static class normalParagraph_return extends ParserRuleReturnScope {
        public StringTemplate st;
        public Object getTemplate() { return st; }
        public String toString() { return st==null?null:st.toString(); }
    };

    // $ANTLR start "normalParagraph"
    // OrgMode.g:77:1: normalParagraph : ( lineText NL )+ ;
    public final OrgModeParser.normalParagraph_return normalParagraph() throws RecognitionException {
        OrgModeParser.normalParagraph_return retval = new OrgModeParser.normalParagraph_return();
        retval.start = input.LT(1);
        int normalParagraph_StartIndex = input.index();
        try {
            if ( state.backtracking>0 && alreadyParsedRule(input, 6) ) { return retval; }
            // OrgMode.g:77:17: ( ( lineText NL )+ )
            // OrgMode.g:77:19: ( lineText NL )+
            {
            // OrgMode.g:77:19: ( lineText NL )+
            int cnt5=0;
            loop5:
            do {
                int alt5=2;
                switch ( input.LA(1) ) {
                case STAR:
                    {
                    int LA5_2 = input.LA(2);

                    if ( (synpred10_OrgMode()) ) {
                        alt5=1;
                    }


                    }
                    break;
                case BEGIN_EXAMPLE:
                    {
                    int LA5_3 = input.LA(2);

                    if ( (synpred10_OrgMode()) ) {
                        alt5=1;
                    }


                    }
                    break;
                case BEGIN_SRC:
                    {
                    int LA5_4 = input.LA(2);

                    if ( (synpred10_OrgMode()) ) {
                        alt5=1;
                    }


                    }
                    break;
                case END_EXAMPLE:
                    {
                    int LA5_5 = input.LA(2);

                    if ( (synpred10_OrgMode()) ) {
                        alt5=1;
                    }


                    }
                    break;
                case END_SRC:
                    {
                    int LA5_6 = input.LA(2);

                    if ( (synpred10_OrgMode()) ) {
                        alt5=1;
                    }


                    }
                    break;
                case PLUS:
                case MINUS:
                case EQ:
                case WORD:
                case WS:
                case BOLD_INLINE:
                case UNDERLINED_INLINE:
                case CODE_INLINE:
                case VERBATIM_INLINE:
                case STRIKE_INLINE:
                case ITALIC_INLINE:
                case LINK_URL:
                case LINK_URL_DESC:
                    {
                    int LA5_7 = input.LA(2);

                    if ( (synpred10_OrgMode()) ) {
                        alt5=1;
                    }


                    }
                    break;

                }

                switch (alt5) {
            	case 1 :
            	    // OrgMode.g:77:20: lineText NL
            	    {
            	    pushFollow(FOLLOW_lineText_in_normalParagraph335);
            	    lineText();

            	    state._fsp--;
            	    if (state.failed) return retval;
            	    match(input,NL,FOLLOW_NL_in_normalParagraph337); if (state.failed) return retval;

            	    }
            	    break;

            	default :
            	    if ( cnt5 >= 1 ) break loop5;
            	    if (state.backtracking>0) {state.failed=true; return retval;}
                        EarlyExitException eee =
                            new EarlyExitException(5, input);
                        throw eee;
                }
                cnt5++;
            } while (true);


            }

            retval.stop = input.LT(-1);

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {
            if ( state.backtracking>0 ) { memoize(input, 6, normalParagraph_StartIndex); }
        }
        return retval;
    }
    // $ANTLR end "normalParagraph"

    public static class lineText_return extends ParserRuleReturnScope {
        public StringTemplate st;
        public Object getTemplate() { return st; }
        public String toString() { return st==null?null:st.toString(); }
    };

    // $ANTLR start "lineText"
    // OrgMode.g:79:1: lineText : ( textItem )+ ;
    public final OrgModeParser.lineText_return lineText() throws RecognitionException {
        OrgModeParser.lineText_return retval = new OrgModeParser.lineText_return();
        retval.start = input.LT(1);
        int lineText_StartIndex = input.index();
        try {
            if ( state.backtracking>0 && alreadyParsedRule(input, 7) ) { return retval; }
            // OrgMode.g:79:10: ( ( textItem )+ )
            // OrgMode.g:79:12: ( textItem )+
            {
            // OrgMode.g:79:12: ( textItem )+
            int cnt6=0;
            loop6:
            do {
                int alt6=2;
                int LA6_0 = input.LA(1);

                if ( ((LA6_0>=BEGIN_EXAMPLE && LA6_0<=EQ)||(LA6_0>=WORD && LA6_0<=LINK_URL_DESC)) ) {
                    alt6=1;
                }


                switch (alt6) {
            	case 1 :
            	    // OrgMode.g:0:0: textItem
            	    {
            	    pushFollow(FOLLOW_textItem_in_lineText348);
            	    textItem();

            	    state._fsp--;
            	    if (state.failed) return retval;

            	    }
            	    break;

            	default :
            	    if ( cnt6 >= 1 ) break loop6;
            	    if (state.backtracking>0) {state.failed=true; return retval;}
                        EarlyExitException eee =
                            new EarlyExitException(6, input);
                        throw eee;
                }
                cnt6++;
            } while (true);


            }

            retval.stop = input.LT(-1);

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {
            if ( state.backtracking>0 ) { memoize(input, 7, lineText_StartIndex); }
        }
        return retval;
    }
    // $ANTLR end "lineText"

    public static class textItem_return extends ParserRuleReturnScope {
        public StringTemplate st;
        public Object getTemplate() { return st; }
        public String toString() { return st==null?null:st.toString(); }
    };

    // $ANTLR start "textItem"
    // OrgMode.g:81:1: textItem : normalText ;
    public final OrgModeParser.textItem_return textItem() throws RecognitionException {
        OrgModeParser.textItem_return retval = new OrgModeParser.textItem_return();
        retval.start = input.LT(1);
        int textItem_StartIndex = input.index();
        try {
            if ( state.backtracking>0 && alreadyParsedRule(input, 8) ) { return retval; }
            // OrgMode.g:81:10: ( normalText )
            // OrgMode.g:81:12: normalText
            {
            pushFollow(FOLLOW_normalText_in_textItem357);
            normalText();

            state._fsp--;
            if (state.failed) return retval;

            }

            retval.stop = input.LT(-1);

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {
            if ( state.backtracking>0 ) { memoize(input, 8, textItem_StartIndex); }
        }
        return retval;
    }
    // $ANTLR end "textItem"

    public static class notNL_return extends ParserRuleReturnScope {
        public StringTemplate st;
        public Object getTemplate() { return st; }
        public String toString() { return st==null?null:st.toString(); }
    };

    // $ANTLR start "notNL"
    // OrgMode.g:84:1: notNL : ~ ( NL | DOUBLE_NL ) ;
    public final OrgModeParser.notNL_return notNL() throws RecognitionException {
        OrgModeParser.notNL_return retval = new OrgModeParser.notNL_return();
        retval.start = input.LT(1);
        int notNL_StartIndex = input.index();
        try {
            if ( state.backtracking>0 && alreadyParsedRule(input, 9) ) { return retval; }
            // OrgMode.g:84:7: (~ ( NL | DOUBLE_NL ) )
            // OrgMode.g:84:9: ~ ( NL | DOUBLE_NL )
            {
            if ( (input.LA(1)>=BEGIN_EXAMPLE && input.LA(1)<=EQ)||(input.LA(1)>=WORD && input.LA(1)<=LINK_URL_DESC) ) {
                input.consume();
                state.errorRecovery=false;state.failed=false;
            }
            else {
                if (state.backtracking>0) {state.failed=true; return retval;}
                MismatchedSetException mse = new MismatchedSetException(null,input);
                throw mse;
            }


            }

            retval.stop = input.LT(-1);

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {
            if ( state.backtracking>0 ) { memoize(input, 9, notNL_StartIndex); }
        }
        return retval;
    }
    // $ANTLR end "notNL"

    public static class normalText_return extends ParserRuleReturnScope {
        public StringTemplate st;
        public Object getTemplate() { return st; }
        public String toString() { return st==null?null:st.toString(); }
    };

    // $ANTLR start "normalText"
    // OrgMode.g:86:1: normalText : notNL ;
    public final OrgModeParser.normalText_return normalText() throws RecognitionException {
        OrgModeParser.normalText_return retval = new OrgModeParser.normalText_return();
        retval.start = input.LT(1);
        int normalText_StartIndex = input.index();
        try {
            if ( state.backtracking>0 && alreadyParsedRule(input, 10) ) { return retval; }
            // OrgMode.g:86:12: ( notNL )
            // OrgMode.g:86:14: notNL
            {
            pushFollow(FOLLOW_notNL_in_normalText380);
            notNL();

            state._fsp--;
            if (state.failed) return retval;
            if ( state.backtracking==0 ) {
              System.out.println("matched normal text: " + input.toString(retval.start,input.LT(-1)));
            }

            }

            retval.stop = input.LT(-1);

        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
        }
        finally {
            if ( state.backtracking>0 ) { memoize(input, 10, normalText_StartIndex); }
        }
        return retval;
    }
    // $ANTLR end "normalText"

    // $ANTLR start synpred2_OrgMode
    public final void synpred2_OrgMode_fragment() throws RecognitionException {   
        // OrgMode.g:62:9: ( header )
        // OrgMode.g:62:9: header
        {
        pushFollow(FOLLOW_header_in_synpred2_OrgMode220);
        header();

        state._fsp--;
        if (state.failed) return ;

        }
    }
    // $ANTLR end synpred2_OrgMode

    // $ANTLR start synpred3_OrgMode
    public final void synpred3_OrgMode_fragment() throws RecognitionException {   
        // OrgMode.g:63:9: ( exampleParagraph )
        // OrgMode.g:63:9: exampleParagraph
        {
        pushFollow(FOLLOW_exampleParagraph_in_synpred3_OrgMode230);
        exampleParagraph();

        state._fsp--;
        if (state.failed) return ;

        }
    }
    // $ANTLR end synpred3_OrgMode

    // $ANTLR start synpred4_OrgMode
    public final void synpred4_OrgMode_fragment() throws RecognitionException {   
        // OrgMode.g:64:9: ( codeParagraph )
        // OrgMode.g:64:9: codeParagraph
        {
        pushFollow(FOLLOW_codeParagraph_in_synpred4_OrgMode240);
        codeParagraph();

        state._fsp--;
        if (state.failed) return ;

        }
    }
    // $ANTLR end synpred4_OrgMode

    // $ANTLR start synpred5_OrgMode
    public final void synpred5_OrgMode_fragment() throws RecognitionException {   
        // OrgMode.g:65:9: ( normalParagraph )
        // OrgMode.g:65:9: normalParagraph
        {
        pushFollow(FOLLOW_normalParagraph_in_synpred5_OrgMode250);
        normalParagraph();

        state._fsp--;
        if (state.failed) return ;

        }
    }
    // $ANTLR end synpred5_OrgMode

    // $ANTLR start synpred7_OrgMode
    public final void synpred7_OrgMode_fragment() throws RecognitionException {   
        // OrgMode.g:70:54: ( STAR )
        // OrgMode.g:70:54: STAR
        {
        match(input,STAR,FOLLOW_STAR_in_synpred7_OrgMode285); if (state.failed) return ;

        }
    }
    // $ANTLR end synpred7_OrgMode

    // $ANTLR start synpred10_OrgMode
    public final void synpred10_OrgMode_fragment() throws RecognitionException {   
        // OrgMode.g:77:20: ( lineText NL )
        // OrgMode.g:77:20: lineText NL
        {
        pushFollow(FOLLOW_lineText_in_synpred10_OrgMode335);
        lineText();

        state._fsp--;
        if (state.failed) return ;
        match(input,NL,FOLLOW_NL_in_synpred10_OrgMode337); if (state.failed) return ;

        }
    }
    // $ANTLR end synpred10_OrgMode

    // Delegated rules

    public final boolean synpred3_OrgMode() {
        state.backtracking++;
        int start = input.mark();
        try {
            synpred3_OrgMode_fragment(); // can never throw exception
        } catch (RecognitionException re) {
            System.err.println("impossible: "+re);
        }
        boolean success = !state.failed;
        input.rewind(start);
        state.backtracking--;
        state.failed=false;
        return success;
    }
    public final boolean synpred5_OrgMode() {
        state.backtracking++;
        int start = input.mark();
        try {
            synpred5_OrgMode_fragment(); // can never throw exception
        } catch (RecognitionException re) {
            System.err.println("impossible: "+re);
        }
        boolean success = !state.failed;
        input.rewind(start);
        state.backtracking--;
        state.failed=false;
        return success;
    }
    public final boolean synpred2_OrgMode() {
        state.backtracking++;
        int start = input.mark();
        try {
            synpred2_OrgMode_fragment(); // can never throw exception
        } catch (RecognitionException re) {
            System.err.println("impossible: "+re);
        }
        boolean success = !state.failed;
        input.rewind(start);
        state.backtracking--;
        state.failed=false;
        return success;
    }
    public final boolean synpred10_OrgMode() {
        state.backtracking++;
        int start = input.mark();
        try {
            synpred10_OrgMode_fragment(); // can never throw exception
        } catch (RecognitionException re) {
            System.err.println("impossible: "+re);
        }
        boolean success = !state.failed;
        input.rewind(start);
        state.backtracking--;
        state.failed=false;
        return success;
    }
    public final boolean synpred4_OrgMode() {
        state.backtracking++;
        int start = input.mark();
        try {
            synpred4_OrgMode_fragment(); // can never throw exception
        } catch (RecognitionException re) {
            System.err.println("impossible: "+re);
        }
        boolean success = !state.failed;
        input.rewind(start);
        state.backtracking--;
        state.failed=false;
        return success;
    }
    public final boolean synpred7_OrgMode() {
        state.backtracking++;
        int start = input.mark();
        try {
            synpred7_OrgMode_fragment(); // can never throw exception
        } catch (RecognitionException re) {
            System.err.println("impossible: "+re);
        }
        boolean success = !state.failed;
        input.rewind(start);
        state.backtracking--;
        state.failed=false;
        return success;
    }


    protected DFA2 dfa2 = new DFA2(this);
    static final String DFA2_eotS =
        "\76\uffff";
    static final String DFA2_eofS =
        "\15\uffff\1\4\6\uffff\1\4\1\uffff\1\4\10\uffff\1\4\2\uffff\1\4\4"+
        "\uffff\3\4\1\uffff\1\4\4\uffff\1\4\3\uffff\1\4\3\uffff\1\4\3\uffff"+
        "\2\4";
    static final String DFA2_minS =
        "\4\4\3\uffff\4\4\1\0\1\uffff\2\4\1\0\1\4\1\0\3\4\1\uffff\4\4\1\0"+
        "\6\4\1\uffff\4\4\1\0\3\4\1\0\2\4\1\0\1\4\1\0\13\4\1\0\2\4";
    static final String DFA2_maxS =
        "\4\27\3\uffff\4\27\1\0\1\uffff\2\27\1\0\1\27\1\0\3\27\1\uffff\4"+
        "\27\1\0\6\27\1\uffff\4\27\1\0\3\27\1\0\2\27\1\0\1\27\1\0\13\27\1"+
        "\0\2\27";
    static final String DFA2_acceptS =
        "\4\uffff\1\4\1\5\1\6\5\uffff\1\1\10\uffff\1\2\13\uffff\1\3\34\uffff";
    static final String DFA2_specialS =
        "\13\uffff\1\4\3\uffff\1\1\1\uffff\1\5\10\uffff\1\7\13\uffff\1\0"+
        "\3\uffff\1\6\2\uffff\1\10\1\uffff\1\3\13\uffff\1\2\2\uffff}>";
    static final String[] DFA2_transitionS = {
            "\1\2\1\4\1\3\1\4\1\1\3\4\1\5\1\6\12\4",
            "\4\10\1\7\3\10\1\4\1\uffff\12\10",
            "\10\11\1\4\1\uffff\12\11",
            "\11\4\1\uffff\1\12\11\4",
            "",
            "",
            "",
            "\4\10\1\7\3\10\1\13\1\14\12\10",
            "\10\10\1\13\1\14\12\10",
            "\10\11\1\15\1\uffff\12\11",
            "\10\16\1\4\1\uffff\12\16",
            "\1\uffff",
            "",
            "\1\21\1\17\1\22\1\23\1\20\3\23\2\4\12\23",
            "\10\16\1\24\1\uffff\12\16",
            "\1\uffff",
            "\4\30\1\27\3\30\1\26\1\uffff\12\30",
            "\1\uffff",
            "\10\23\1\26\1\uffff\1\31\11\23",
            "\10\23\1\26\1\uffff\12\23",
            "\1\34\1\36\1\35\1\32\1\33\3\36\2\4\12\36",
            "",
            "\1\21\1\17\1\22\1\23\1\20\3\23\2\4\12\23",
            "\4\30\1\27\3\30\1\37\1\4\12\30",
            "\10\30\1\37\1\4\12\30",
            "\10\40\1\26\1\uffff\12\40",
            "\1\uffff",
            "\4\44\1\43\3\44\1\42\1\uffff\12\44",
            "\10\45\1\42\1\uffff\12\45",
            "\10\36\1\42\1\uffff\1\46\11\36",
            "\10\36\1\42\1\uffff\12\36",
            "\1\21\1\17\1\22\1\23\1\20\3\23\2\4\12\23",
            "\10\40\1\47\1\uffff\12\40",
            "",
            "\1\34\1\36\1\35\1\32\1\33\3\36\2\4\12\36",
            "\4\44\1\43\3\44\1\50\1\4\12\44",
            "\10\44\1\50\1\4\12\44",
            "\10\45\1\51\1\uffff\12\45",
            "\1\uffff",
            "\1\55\1\52\1\56\1\53\1\54\3\40\2\4\12\40",
            "\1\34\1\36\1\35\1\32\1\33\3\36\2\4\12\36",
            "\1\62\1\60\1\63\1\57\1\61\3\45\2\4\12\45",
            "\1\uffff",
            "\1\55\1\40\1\56\1\40\1\54\3\40\1\64\1\4\12\40",
            "\4\66\1\65\3\66\1\47\1\uffff\12\66",
            "\1\uffff",
            "\10\40\1\47\1\uffff\1\67\11\40",
            "\1\uffff",
            "\1\62\1\45\1\63\1\45\1\61\3\45\1\70\1\4\12\45",
            "\4\72\1\71\3\72\1\51\1\uffff\12\72",
            "\10\45\1\51\1\uffff\12\45",
            "\10\45\1\51\1\uffff\1\73\11\45",
            "\1\55\1\52\1\56\1\53\1\54\3\40\2\4\12\40",
            "\4\66\1\65\3\66\1\74\1\4\12\66",
            "\10\66\1\74\1\4\12\66",
            "\10\40\1\47\1\uffff\12\40",
            "\1\62\1\60\1\63\1\57\1\61\3\45\2\4\12\45",
            "\4\72\1\71\3\72\1\75\1\4\12\72",
            "\10\72\1\75\1\4\12\72",
            "\1\uffff",
            "\1\55\1\52\1\56\1\53\1\54\3\40\2\4\12\40",
            "\1\62\1\60\1\63\1\57\1\61\3\45\2\4\12\45"
    };

    static final short[] DFA2_eot = DFA.unpackEncodedString(DFA2_eotS);
    static final short[] DFA2_eof = DFA.unpackEncodedString(DFA2_eofS);
    static final char[] DFA2_min = DFA.unpackEncodedStringToUnsignedChars(DFA2_minS);
    static final char[] DFA2_max = DFA.unpackEncodedStringToUnsignedChars(DFA2_maxS);
    static final short[] DFA2_accept = DFA.unpackEncodedString(DFA2_acceptS);
    static final short[] DFA2_special = DFA.unpackEncodedString(DFA2_specialS);
    static final short[][] DFA2_transition;

    static {
        int numStates = DFA2_transitionS.length;
        DFA2_transition = new short[numStates][];
        for (int i=0; i<numStates; i++) {
            DFA2_transition[i] = DFA.unpackEncodedString(DFA2_transitionS[i]);
        }
    }

    class DFA2 extends DFA {

        public DFA2(BaseRecognizer recognizer) {
            this.recognizer = recognizer;
            this.decisionNumber = 2;
            this.eot = DFA2_eot;
            this.eof = DFA2_eof;
            this.min = DFA2_min;
            this.max = DFA2_max;
            this.accept = DFA2_accept;
            this.special = DFA2_special;
            this.transition = DFA2_transition;
        }
        public String getDescription() {
            return "61:1: block : ( header | exampleParagraph | codeParagraph | normalParagraph | NL | DOUBLE_NL );";
        }
        public int specialStateTransition(int s, IntStream _input) throws NoViableAltException {
            TokenStream input = (TokenStream)_input;
        	int _s = s;
            switch ( s ) {
                    case 0 : 
                        int LA2_38 = input.LA(1);

                         
                        int index2_38 = input.index();
                        input.rewind();
                        s = -1;
                        if ( (synpred4_OrgMode()) ) {s = 33;}

                        else if ( (synpred5_OrgMode()) ) {s = 4;}

                         
                        input.seek(index2_38);
                        if ( s>=0 ) return s;
                        break;
                    case 1 : 
                        int LA2_15 = input.LA(1);

                         
                        int index2_15 = input.index();
                        input.rewind();
                        s = -1;
                        if ( (synpred3_OrgMode()) ) {s = 21;}

                        else if ( (synpred5_OrgMode()) ) {s = 4;}

                         
                        input.seek(index2_15);
                        if ( s>=0 ) return s;
                        break;
                    case 2 : 
                        int LA2_59 = input.LA(1);

                         
                        int index2_59 = input.index();
                        input.rewind();
                        s = -1;
                        if ( (synpred4_OrgMode()) ) {s = 33;}

                        else if ( (synpred5_OrgMode()) ) {s = 4;}

                         
                        input.seek(index2_59);
                        if ( s>=0 ) return s;
                        break;
                    case 3 : 
                        int LA2_47 = input.LA(1);

                         
                        int index2_47 = input.index();
                        input.rewind();
                        s = -1;
                        if ( (synpred4_OrgMode()) ) {s = 33;}

                        else if ( (synpred5_OrgMode()) ) {s = 4;}

                         
                        input.seek(index2_47);
                        if ( s>=0 ) return s;
                        break;
                    case 4 : 
                        int LA2_11 = input.LA(1);

                         
                        int index2_11 = input.index();
                        input.rewind();
                        s = -1;
                        if ( ((synpred2_OrgMode()&&(input.LT(1).getCharPositionInLine() == 0))) ) {s = 12;}

                        else if ( (synpred5_OrgMode()) ) {s = 4;}

                         
                        input.seek(index2_11);
                        if ( s>=0 ) return s;
                        break;
                    case 5 : 
                        int LA2_17 = input.LA(1);

                         
                        int index2_17 = input.index();
                        input.rewind();
                        s = -1;
                        if ( (synpred3_OrgMode()) ) {s = 21;}

                        else if ( (synpred5_OrgMode()) ) {s = 4;}

                         
                        input.seek(index2_17);
                        if ( s>=0 ) return s;
                        break;
                    case 6 : 
                        int LA2_42 = input.LA(1);

                         
                        int index2_42 = input.index();
                        input.rewind();
                        s = -1;
                        if ( (synpred3_OrgMode()) ) {s = 21;}

                        else if ( (synpred5_OrgMode()) ) {s = 4;}

                         
                        input.seek(index2_42);
                        if ( s>=0 ) return s;
                        break;
                    case 7 : 
                        int LA2_26 = input.LA(1);

                         
                        int index2_26 = input.index();
                        input.rewind();
                        s = -1;
                        if ( (synpred4_OrgMode()) ) {s = 33;}

                        else if ( (synpred5_OrgMode()) ) {s = 4;}

                         
                        input.seek(index2_26);
                        if ( s>=0 ) return s;
                        break;
                    case 8 : 
                        int LA2_45 = input.LA(1);

                         
                        int index2_45 = input.index();
                        input.rewind();
                        s = -1;
                        if ( (synpred3_OrgMode()) ) {s = 21;}

                        else if ( (synpred5_OrgMode()) ) {s = 4;}

                         
                        input.seek(index2_45);
                        if ( s>=0 ) return s;
                        break;
            }
            if (state.backtracking>0) {state.failed=true; return -1;}
            NoViableAltException nvae =
                new NoViableAltException(getDescription(), 2, _s, input);
            error(nvae);
            throw nvae;
        }
    }
 

    public static final BitSet FOLLOW_block_in_orgFile202 = new BitSet(new long[]{0x0000000000FFFFF2L});
    public static final BitSet FOLLOW_header_in_block220 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_exampleParagraph_in_block230 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_codeParagraph_in_block240 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_normalParagraph_in_block250 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_NL_in_block260 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_DOUBLE_NL_in_block270 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_STAR_in_header285 = new BitSet(new long[]{0x0000000000FFCFF0L});
    public static final BitSet FOLLOW_notNL_in_header288 = new BitSet(new long[]{0x0000000000FFFFF0L});
    public static final BitSet FOLLOW_set_in_header291 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_BEGIN_EXAMPLE_in_exampleParagraph305 = new BitSet(new long[]{0x0000000000FFCFF0L});
    public static final BitSet FOLLOW_normalParagraph_in_exampleParagraph307 = new BitSet(new long[]{0x0000000000000020L});
    public static final BitSet FOLLOW_END_EXAMPLE_in_exampleParagraph309 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_BEGIN_SRC_in_codeParagraph318 = new BitSet(new long[]{0x0000000000004000L});
    public static final BitSet FOLLOW_WORD_in_codeParagraph320 = new BitSet(new long[]{0x0000000000FFCFF0L});
    public static final BitSet FOLLOW_normalParagraph_in_codeParagraph322 = new BitSet(new long[]{0x0000000000000080L});
    public static final BitSet FOLLOW_END_SRC_in_codeParagraph324 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_lineText_in_normalParagraph335 = new BitSet(new long[]{0x0000000000001000L});
    public static final BitSet FOLLOW_NL_in_normalParagraph337 = new BitSet(new long[]{0x0000000000FFCFF2L});
    public static final BitSet FOLLOW_textItem_in_lineText348 = new BitSet(new long[]{0x0000000000FFCFF2L});
    public static final BitSet FOLLOW_normalText_in_textItem357 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_set_in_notNL366 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_notNL_in_normalText380 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_header_in_synpred2_OrgMode220 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_exampleParagraph_in_synpred3_OrgMode230 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_codeParagraph_in_synpred4_OrgMode240 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_normalParagraph_in_synpred5_OrgMode250 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_STAR_in_synpred7_OrgMode285 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_lineText_in_synpred10_OrgMode335 = new BitSet(new long[]{0x0000000000001000L});
    public static final BitSet FOLLOW_NL_in_synpred10_OrgMode337 = new BitSet(new long[]{0x0000000000000002L});

}