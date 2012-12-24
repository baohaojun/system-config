// $ANTLR 3.2 debian-7 OrgMode.g 2012-12-21 23:55:50

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
        "<invalid>", "<EOR>", "<DOWN>", "<UP>", "BEGIN_EXAMPLE", "END_EXAMPLE", "BEGIN_SRC", "END_SRC", "NL", "HEADER_STAR", "BEG_EXAMPLE", "WORDF", "BEG_BLOCK", "END_BLOCK", "WS", "WSF", "AWSF", "BOLD_INLINE", "UNDERLINED_INLINE", "CODE_INLINE", "VERBATIM_INLINE", "STRIKE_INLINE", "ITALIC_INLINE", "LINK_URL", "LINK_URL_DESC", "LE", "OL_START", "UL_START", "COL_START", "SHARP_SETTING", "WORD_NWS", "NWS"
    };
    public static final int UL_START=27;
    public static final int BEG_BLOCK=12;
    public static final int BEGIN_EXAMPLE=4;
    public static final int SHARP_SETTING=29;
    public static final int WSF=15;
    public static final int LINK_URL=23;
    public static final int AWSF=16;
    public static final int STRIKE_INLINE=21;
    public static final int WORD_NWS=30;
    public static final int COL_START=28;
    public static final int WORDF=11;
    public static final int CODE_INLINE=19;
    public static final int EOF=-1;
    public static final int NWS=31;
    public static final int BOLD_INLINE=17;
    public static final int BEG_EXAMPLE=10;
    public static final int HEADER_STAR=9;
    public static final int VERBATIM_INLINE=20;
    public static final int WS=14;
    public static final int END_BLOCK=13;
    public static final int UNDERLINED_INLINE=18;
    public static final int END_EXAMPLE=5;
    public static final int END_SRC=7;
    public static final int OL_START=26;
    public static final int LINK_URL_DESC=24;
    public static final int ITALIC_INLINE=22;
    public static final int NL=8;
    public static final int BEGIN_SRC=6;
    public static final int LE=25;

    // delegates
    // delegators


        public OrgModeParser(TokenStream input) {
            this(input, new RecognizerSharedState());
        }
        public OrgModeParser(TokenStream input, RecognizerSharedState state) {
            super(input, state);
            this.state.ruleMemo = new HashMap[17+1];
             
             
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
    // OrgMode.g:78:1: orgFile : ( block )* ;
    public final OrgModeParser.orgFile_return orgFile() throws RecognitionException {
        OrgModeParser.orgFile_return retval = new OrgModeParser.orgFile_return();
        retval.start = input.LT(1);
        int orgFile_StartIndex = input.index();
        try {
            if ( state.backtracking>0 && alreadyParsedRule(input, 1) ) { return retval; }
            // OrgMode.g:78:9: ( ( block )* )
            // OrgMode.g:78:11: ( block )*
            {
            // OrgMode.g:78:11: ( block )*
            loop1:
            do {
                int alt1=2;
                int LA1_0 = input.LA(1);

                if ( ((LA1_0>=BEGIN_EXAMPLE && LA1_0<=NWS)) ) {
                    alt1=1;
                }


                switch (alt1) {
            	case 1 :
            	    // OrgMode.g:0:0: block
            	    {
            	    pushFollow(FOLLOW_block_in_orgFile126);
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
    // OrgMode.g:80:1: block : ( header | exampleParagraph | normalParagraph | NL );
    public final OrgModeParser.block_return block() throws RecognitionException {
        OrgModeParser.block_return retval = new OrgModeParser.block_return();
        retval.start = input.LT(1);
        int block_StartIndex = input.index();
        try {
            if ( state.backtracking>0 && alreadyParsedRule(input, 2) ) { return retval; }
            // OrgMode.g:80:7: ( header | exampleParagraph | normalParagraph | NL )
            int alt2=4;
            alt2 = dfa2.predict(input);
            switch (alt2) {
                case 1 :
                    // OrgMode.g:81:9: header
                    {
                    pushFollow(FOLLOW_header_in_block144);
                    header();

                    state._fsp--;
                    if (state.failed) return retval;

                    }
                    break;
                case 2 :
                    // OrgMode.g:82:9: exampleParagraph
                    {
                    pushFollow(FOLLOW_exampleParagraph_in_block154);
                    exampleParagraph();

                    state._fsp--;
                    if (state.failed) return retval;

                    }
                    break;
                case 3 :
                    // OrgMode.g:83:9: normalParagraph
                    {
                    pushFollow(FOLLOW_normalParagraph_in_block164);
                    normalParagraph();

                    state._fsp--;
                    if (state.failed) return retval;

                    }
                    break;
                case 4 :
                    // OrgMode.g:84:9: NL
                    {
                    match(input,NL,FOLLOW_NL_in_block174); if (state.failed) return retval;

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
    // OrgMode.g:86:1: header : HEADER_STAR ( notNL )+ NL ;
    public final OrgModeParser.header_return header() throws RecognitionException {
        OrgModeParser.header_return retval = new OrgModeParser.header_return();
        retval.start = input.LT(1);
        int header_StartIndex = input.index();
        try {
            if ( state.backtracking>0 && alreadyParsedRule(input, 3) ) { return retval; }
            // OrgMode.g:86:8: ( HEADER_STAR ( notNL )+ NL )
            // OrgMode.g:86:10: HEADER_STAR ( notNL )+ NL
            {
            match(input,HEADER_STAR,FOLLOW_HEADER_STAR_in_header186); if (state.failed) return retval;
            // OrgMode.g:86:22: ( notNL )+
            int cnt3=0;
            loop3:
            do {
                int alt3=2;
                int LA3_0 = input.LA(1);

                if ( ((LA3_0>=BEGIN_EXAMPLE && LA3_0<=END_SRC)||(LA3_0>=HEADER_STAR && LA3_0<=NWS)) ) {
                    alt3=1;
                }


                switch (alt3) {
            	case 1 :
            	    // OrgMode.g:0:0: notNL
            	    {
            	    pushFollow(FOLLOW_notNL_in_header188);
            	    notNL();

            	    state._fsp--;
            	    if (state.failed) return retval;

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

            match(input,NL,FOLLOW_NL_in_header191); if (state.failed) return retval;
            if ( state.backtracking==0 ) {
              System.out.println("got a header " + input.toString(retval.start,input.LT(-1)) + " end of header");
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
    // OrgMode.g:88:1: exampleParagraph : BEG_EXAMPLE ( . )* END_EXAMPLE ;
    public final OrgModeParser.exampleParagraph_return exampleParagraph() throws RecognitionException {
        OrgModeParser.exampleParagraph_return retval = new OrgModeParser.exampleParagraph_return();
        retval.start = input.LT(1);
        int exampleParagraph_StartIndex = input.index();
        try {
            if ( state.backtracking>0 && alreadyParsedRule(input, 4) ) { return retval; }
            // OrgMode.g:88:18: ( BEG_EXAMPLE ( . )* END_EXAMPLE )
            // OrgMode.g:88:20: BEG_EXAMPLE ( . )* END_EXAMPLE
            {
            match(input,BEG_EXAMPLE,FOLLOW_BEG_EXAMPLE_in_exampleParagraph201); if (state.failed) return retval;
            // OrgMode.g:88:32: ( . )*
            loop4:
            do {
                int alt4=2;
                alt4 = dfa4.predict(input);
                switch (alt4) {
            	case 1 :
            	    // OrgMode.g:0:0: .
            	    {
            	    matchAny(input); if (state.failed) return retval;

            	    }
            	    break;

            	default :
            	    break loop4;
                }
            } while (true);

            match(input,END_EXAMPLE,FOLLOW_END_EXAMPLE_in_exampleParagraph206); if (state.failed) return retval;
            if ( state.backtracking==0 ) {
              System.out.println("matched exampleParagraph: " + input.toString(retval.start,input.LT(-1)));
            }

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

    public static class normalParagraph_return extends ParserRuleReturnScope {
        public StringTemplate st;
        public Object getTemplate() { return st; }
        public String toString() { return st==null?null:st.toString(); }
    };

    // $ANTLR start "normalParagraph"
    // OrgMode.g:90:1: normalParagraph : ( lineText NL )+ ;
    public final OrgModeParser.normalParagraph_return normalParagraph() throws RecognitionException {
        OrgModeParser.normalParagraph_return retval = new OrgModeParser.normalParagraph_return();
        retval.start = input.LT(1);
        int normalParagraph_StartIndex = input.index();
        try {
            if ( state.backtracking>0 && alreadyParsedRule(input, 5) ) { return retval; }
            // OrgMode.g:90:17: ( ( lineText NL )+ )
            // OrgMode.g:90:19: ( lineText NL )+
            {
            // OrgMode.g:90:19: ( lineText NL )+
            int cnt5=0;
            loop5:
            do {
                int alt5=2;
                switch ( input.LA(1) ) {
                case HEADER_STAR:
                    {
                    int LA5_2 = input.LA(2);

                    if ( (synpred7_OrgMode()) ) {
                        alt5=1;
                    }


                    }
                    break;
                case BEG_EXAMPLE:
                    {
                    int LA5_3 = input.LA(2);

                    if ( (synpred7_OrgMode()) ) {
                        alt5=1;
                    }


                    }
                    break;
                case BEGIN_EXAMPLE:
                case END_EXAMPLE:
                case BEGIN_SRC:
                case END_SRC:
                case WORDF:
                case BEG_BLOCK:
                case END_BLOCK:
                case WS:
                case WSF:
                case AWSF:
                case BOLD_INLINE:
                case UNDERLINED_INLINE:
                case CODE_INLINE:
                case VERBATIM_INLINE:
                case STRIKE_INLINE:
                case ITALIC_INLINE:
                case LINK_URL:
                case LINK_URL_DESC:
                case LE:
                case OL_START:
                case UL_START:
                case COL_START:
                case SHARP_SETTING:
                case WORD_NWS:
                case NWS:
                    {
                    int LA5_4 = input.LA(2);

                    if ( (synpred7_OrgMode()) ) {
                        alt5=1;
                    }


                    }
                    break;

                }

                switch (alt5) {
            	case 1 :
            	    // OrgMode.g:90:20: lineText NL
            	    {
            	    pushFollow(FOLLOW_lineText_in_normalParagraph218);
            	    lineText();

            	    state._fsp--;
            	    if (state.failed) return retval;
            	    match(input,NL,FOLLOW_NL_in_normalParagraph220); if (state.failed) return retval;

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
            if ( state.backtracking>0 ) { memoize(input, 5, normalParagraph_StartIndex); }
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
    // OrgMode.g:92:1: lineText : ( textItem )+ ;
    public final OrgModeParser.lineText_return lineText() throws RecognitionException {
        OrgModeParser.lineText_return retval = new OrgModeParser.lineText_return();
        retval.start = input.LT(1);
        int lineText_StartIndex = input.index();
        try {
            if ( state.backtracking>0 && alreadyParsedRule(input, 6) ) { return retval; }
            // OrgMode.g:92:10: ( ( textItem )+ )
            // OrgMode.g:92:12: ( textItem )+
            {
            // OrgMode.g:92:12: ( textItem )+
            int cnt6=0;
            loop6:
            do {
                int alt6=2;
                int LA6_0 = input.LA(1);

                if ( ((LA6_0>=BEGIN_EXAMPLE && LA6_0<=END_SRC)||(LA6_0>=HEADER_STAR && LA6_0<=NWS)) ) {
                    alt6=1;
                }


                switch (alt6) {
            	case 1 :
            	    // OrgMode.g:0:0: textItem
            	    {
            	    pushFollow(FOLLOW_textItem_in_lineText231);
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
            if ( state.backtracking>0 ) { memoize(input, 6, lineText_StartIndex); }
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
    // OrgMode.g:94:1: textItem : normalText ;
    public final OrgModeParser.textItem_return textItem() throws RecognitionException {
        OrgModeParser.textItem_return retval = new OrgModeParser.textItem_return();
        retval.start = input.LT(1);
        int textItem_StartIndex = input.index();
        try {
            if ( state.backtracking>0 && alreadyParsedRule(input, 7) ) { return retval; }
            // OrgMode.g:94:10: ( normalText )
            // OrgMode.g:94:12: normalText
            {
            pushFollow(FOLLOW_normalText_in_textItem240);
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
            if ( state.backtracking>0 ) { memoize(input, 7, textItem_StartIndex); }
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
    // OrgMode.g:97:1: notNL : ~ NL ;
    public final OrgModeParser.notNL_return notNL() throws RecognitionException {
        OrgModeParser.notNL_return retval = new OrgModeParser.notNL_return();
        retval.start = input.LT(1);
        int notNL_StartIndex = input.index();
        try {
            if ( state.backtracking>0 && alreadyParsedRule(input, 8) ) { return retval; }
            // OrgMode.g:97:7: (~ NL )
            // OrgMode.g:97:9: ~ NL
            {
            if ( (input.LA(1)>=BEGIN_EXAMPLE && input.LA(1)<=END_SRC)||(input.LA(1)>=HEADER_STAR && input.LA(1)<=NWS) ) {
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
            if ( state.backtracking>0 ) { memoize(input, 8, notNL_StartIndex); }
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
    // OrgMode.g:99:1: normalText : notNL ;
    public final OrgModeParser.normalText_return normalText() throws RecognitionException {
        OrgModeParser.normalText_return retval = new OrgModeParser.normalText_return();
        retval.start = input.LT(1);
        int normalText_StartIndex = input.index();
        try {
            if ( state.backtracking>0 && alreadyParsedRule(input, 9) ) { return retval; }
            // OrgMode.g:99:12: ( notNL )
            // OrgMode.g:99:14: notNL
            {
            pushFollow(FOLLOW_notNL_in_normalText259);
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
            if ( state.backtracking>0 ) { memoize(input, 9, normalText_StartIndex); }
        }
        return retval;
    }
    // $ANTLR end "normalText"

    // $ANTLR start synpred2_OrgMode
    public final void synpred2_OrgMode_fragment() throws RecognitionException {   
        // OrgMode.g:81:9: ( header )
        // OrgMode.g:81:9: header
        {
        pushFollow(FOLLOW_header_in_synpred2_OrgMode144);
        header();

        state._fsp--;
        if (state.failed) return ;

        }
    }
    // $ANTLR end synpred2_OrgMode

    // $ANTLR start synpred3_OrgMode
    public final void synpred3_OrgMode_fragment() throws RecognitionException {   
        // OrgMode.g:82:9: ( exampleParagraph )
        // OrgMode.g:82:9: exampleParagraph
        {
        pushFollow(FOLLOW_exampleParagraph_in_synpred3_OrgMode154);
        exampleParagraph();

        state._fsp--;
        if (state.failed) return ;

        }
    }
    // $ANTLR end synpred3_OrgMode

    // $ANTLR start synpred4_OrgMode
    public final void synpred4_OrgMode_fragment() throws RecognitionException {   
        // OrgMode.g:83:9: ( normalParagraph )
        // OrgMode.g:83:9: normalParagraph
        {
        pushFollow(FOLLOW_normalParagraph_in_synpred4_OrgMode164);
        normalParagraph();

        state._fsp--;
        if (state.failed) return ;

        }
    }
    // $ANTLR end synpred4_OrgMode

    // $ANTLR start synpred6_OrgMode
    public final void synpred6_OrgMode_fragment() throws RecognitionException {   
        // OrgMode.g:88:32: ( . )
        // OrgMode.g:88:32: .
        {
        matchAny(input); if (state.failed) return ;

        }
    }
    // $ANTLR end synpred6_OrgMode

    // $ANTLR start synpred7_OrgMode
    public final void synpred7_OrgMode_fragment() throws RecognitionException {   
        // OrgMode.g:90:20: ( lineText NL )
        // OrgMode.g:90:20: lineText NL
        {
        pushFollow(FOLLOW_lineText_in_synpred7_OrgMode218);
        lineText();

        state._fsp--;
        if (state.failed) return ;
        match(input,NL,FOLLOW_NL_in_synpred7_OrgMode220); if (state.failed) return ;

        }
    }
    // $ANTLR end synpred7_OrgMode

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
    public final boolean synpred6_OrgMode() {
        state.backtracking++;
        int start = input.mark();
        try {
            synpred6_OrgMode_fragment(); // can never throw exception
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
    protected DFA4 dfa4 = new DFA4(this);
    static final String DFA2_eotS =
        "\35\uffff";
    static final String DFA2_eofS =
        "\7\uffff\1\3\7\uffff\1\3\2\uffff\1\3\5\uffff\1\3\2\uffff\2\3";
    static final String DFA2_minS =
        "\3\4\2\uffff\1\4\1\0\2\4\1\0\1\uffff\1\0\1\4\1\0\2\4\1\uffff\1\0"+
        "\2\4\1\0\1\4\1\0\2\4\1\0\3\4";
    static final String DFA2_maxS =
        "\3\37\2\uffff\1\37\1\0\2\37\1\0\1\uffff\1\0\1\37\1\0\2\37\1\uffff"+
        "\1\0\2\37\1\0\1\37\1\0\2\37\1\0\3\37";
    static final String DFA2_acceptS =
        "\3\uffff\1\3\1\4\5\uffff\1\2\5\uffff\1\1\14\uffff";
    static final String DFA2_specialS =
        "\6\uffff\1\6\2\uffff\1\4\1\uffff\1\3\1\uffff\1\2\3\uffff\1\0\2\uffff"+
        "\1\7\1\uffff\1\1\2\uffff\1\5\3\uffff}>";
    static final String[] DFA2_transitionS = {
            "\4\3\1\4\1\1\1\2\25\3",
            "\4\5\1\3\27\5",
            "\1\10\1\6\2\10\1\7\27\10",
            "",
            "",
            "\4\5\1\11\27\5",
            "\1\uffff",
            "\1\16\1\13\2\16\1\17\1\14\1\15\25\16",
            "\1\10\1\6\2\10\1\7\27\10",
            "\1\uffff",
            "",
            "\1\uffff",
            "\1\23\1\21\2\23\1\22\27\23",
            "\1\uffff",
            "\1\16\1\13\2\16\1\22\27\16",
            "\1\27\1\24\2\27\1\17\1\25\1\26\25\27",
            "",
            "\1\uffff",
            "\1\16\1\13\2\16\1\17\1\14\1\15\25\16",
            "\1\23\1\21\2\23\1\30\27\23",
            "\1\uffff",
            "\1\32\1\31\2\32\1\33\27\32",
            "\1\uffff",
            "\1\27\1\24\2\27\1\33\27\27",
            "\1\16\1\13\2\16\1\17\1\14\1\15\25\16",
            "\1\uffff",
            "\1\32\1\31\2\32\1\34\27\32",
            "\1\27\1\24\2\27\1\17\1\25\1\26\25\27",
            "\1\27\1\24\2\27\1\17\1\25\1\26\25\27"
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
            return "80:1: block : ( header | exampleParagraph | normalParagraph | NL );";
        }
        public int specialStateTransition(int s, IntStream _input) throws NoViableAltException {
            TokenStream input = (TokenStream)_input;
        	int _s = s;
            switch ( s ) {
                    case 0 : 
                        int LA2_17 = input.LA(1);

                         
                        int index2_17 = input.index();
                        input.rewind();
                        s = -1;
                        if ( (synpred3_OrgMode()) ) {s = 10;}

                        else if ( (synpred4_OrgMode()) ) {s = 3;}

                         
                        input.seek(index2_17);
                        if ( s>=0 ) return s;
                        break;
                    case 1 : 
                        int LA2_22 = input.LA(1);

                         
                        int index2_22 = input.index();
                        input.rewind();
                        s = -1;
                        if ( (synpred3_OrgMode()) ) {s = 10;}

                        else if ( (synpred4_OrgMode()) ) {s = 3;}

                         
                        input.seek(index2_22);
                        if ( s>=0 ) return s;
                        break;
                    case 2 : 
                        int LA2_13 = input.LA(1);

                         
                        int index2_13 = input.index();
                        input.rewind();
                        s = -1;
                        if ( (synpred3_OrgMode()) ) {s = 10;}

                        else if ( (synpred4_OrgMode()) ) {s = 3;}

                         
                        input.seek(index2_13);
                        if ( s>=0 ) return s;
                        break;
                    case 3 : 
                        int LA2_11 = input.LA(1);

                         
                        int index2_11 = input.index();
                        input.rewind();
                        s = -1;
                        if ( (synpred3_OrgMode()) ) {s = 10;}

                        else if ( (synpred4_OrgMode()) ) {s = 3;}

                         
                        input.seek(index2_11);
                        if ( s>=0 ) return s;
                        break;
                    case 4 : 
                        int LA2_9 = input.LA(1);

                         
                        int index2_9 = input.index();
                        input.rewind();
                        s = -1;
                        if ( (synpred2_OrgMode()) ) {s = 16;}

                        else if ( (synpred4_OrgMode()) ) {s = 3;}

                         
                        input.seek(index2_9);
                        if ( s>=0 ) return s;
                        break;
                    case 5 : 
                        int LA2_25 = input.LA(1);

                         
                        int index2_25 = input.index();
                        input.rewind();
                        s = -1;
                        if ( (synpred3_OrgMode()) ) {s = 10;}

                        else if ( (synpred4_OrgMode()) ) {s = 3;}

                         
                        input.seek(index2_25);
                        if ( s>=0 ) return s;
                        break;
                    case 6 : 
                        int LA2_6 = input.LA(1);

                         
                        int index2_6 = input.index();
                        input.rewind();
                        s = -1;
                        if ( (synpred3_OrgMode()) ) {s = 10;}

                        else if ( (synpred4_OrgMode()) ) {s = 3;}

                         
                        input.seek(index2_6);
                        if ( s>=0 ) return s;
                        break;
                    case 7 : 
                        int LA2_20 = input.LA(1);

                         
                        int index2_20 = input.index();
                        input.rewind();
                        s = -1;
                        if ( (synpred3_OrgMode()) ) {s = 10;}

                        else if ( (synpred4_OrgMode()) ) {s = 3;}

                         
                        input.seek(index2_20);
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
    static final String DFA4_eotS =
        "\15\uffff";
    static final String DFA4_eofS =
        "\1\uffff\1\5\6\uffff\1\5\2\uffff\2\5";
    static final String DFA4_minS =
        "\2\4\1\uffff\1\0\1\4\1\uffff\1\0\2\4\1\0\3\4";
    static final String DFA4_maxS =
        "\2\37\1\uffff\1\0\1\37\1\uffff\1\0\2\37\1\0\3\37";
    static final String DFA4_acceptS =
        "\2\uffff\1\1\2\uffff\1\2\7\uffff";
    static final String DFA4_specialS =
        "\3\uffff\1\2\2\uffff\1\0\2\uffff\1\1\3\uffff}>";
    static final String[] DFA4_transitionS = {
            "\1\2\1\1\32\2",
            "\1\7\1\3\2\7\1\10\1\4\1\6\25\7",
            "",
            "\1\uffff",
            "\1\12\1\11\2\12\1\13\27\12",
            "",
            "\1\uffff",
            "\1\7\1\3\2\7\1\13\27\7",
            "\1\7\1\3\2\7\1\10\1\4\1\6\25\7",
            "\1\uffff",
            "\1\12\1\11\2\12\1\14\27\12",
            "\1\7\1\3\2\7\1\10\1\4\1\6\25\7",
            "\1\7\1\3\2\7\1\10\1\4\1\6\25\7"
    };

    static final short[] DFA4_eot = DFA.unpackEncodedString(DFA4_eotS);
    static final short[] DFA4_eof = DFA.unpackEncodedString(DFA4_eofS);
    static final char[] DFA4_min = DFA.unpackEncodedStringToUnsignedChars(DFA4_minS);
    static final char[] DFA4_max = DFA.unpackEncodedStringToUnsignedChars(DFA4_maxS);
    static final short[] DFA4_accept = DFA.unpackEncodedString(DFA4_acceptS);
    static final short[] DFA4_special = DFA.unpackEncodedString(DFA4_specialS);
    static final short[][] DFA4_transition;

    static {
        int numStates = DFA4_transitionS.length;
        DFA4_transition = new short[numStates][];
        for (int i=0; i<numStates; i++) {
            DFA4_transition[i] = DFA.unpackEncodedString(DFA4_transitionS[i]);
        }
    }

    class DFA4 extends DFA {

        public DFA4(BaseRecognizer recognizer) {
            this.recognizer = recognizer;
            this.decisionNumber = 4;
            this.eot = DFA4_eot;
            this.eof = DFA4_eof;
            this.min = DFA4_min;
            this.max = DFA4_max;
            this.accept = DFA4_accept;
            this.special = DFA4_special;
            this.transition = DFA4_transition;
        }
        public String getDescription() {
            return "()* loopback of 88:32: ( . )*";
        }
        public int specialStateTransition(int s, IntStream _input) throws NoViableAltException {
            TokenStream input = (TokenStream)_input;
        	int _s = s;
            switch ( s ) {
                    case 0 : 
                        int LA4_6 = input.LA(1);

                         
                        int index4_6 = input.index();
                        input.rewind();
                        s = -1;
                        if ( (synpred6_OrgMode()) ) {s = 2;}

                        else if ( (true) ) {s = 5;}

                         
                        input.seek(index4_6);
                        if ( s>=0 ) return s;
                        break;
                    case 1 : 
                        int LA4_9 = input.LA(1);

                         
                        int index4_9 = input.index();
                        input.rewind();
                        s = -1;
                        if ( (synpred6_OrgMode()) ) {s = 2;}

                        else if ( (true) ) {s = 5;}

                         
                        input.seek(index4_9);
                        if ( s>=0 ) return s;
                        break;
                    case 2 : 
                        int LA4_3 = input.LA(1);

                         
                        int index4_3 = input.index();
                        input.rewind();
                        s = -1;
                        if ( (synpred6_OrgMode()) ) {s = 2;}

                        else if ( (true) ) {s = 5;}

                         
                        input.seek(index4_3);
                        if ( s>=0 ) return s;
                        break;
            }
            if (state.backtracking>0) {state.failed=true; return -1;}
            NoViableAltException nvae =
                new NoViableAltException(getDescription(), 4, _s, input);
            error(nvae);
            throw nvae;
        }
    }
 

    public static final BitSet FOLLOW_block_in_orgFile126 = new BitSet(new long[]{0x00000000FFFFFFF2L});
    public static final BitSet FOLLOW_header_in_block144 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_exampleParagraph_in_block154 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_normalParagraph_in_block164 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_NL_in_block174 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_HEADER_STAR_in_header186 = new BitSet(new long[]{0x00000000FFFFFEF0L});
    public static final BitSet FOLLOW_notNL_in_header188 = new BitSet(new long[]{0x00000000FFFFFFF0L});
    public static final BitSet FOLLOW_NL_in_header191 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_BEG_EXAMPLE_in_exampleParagraph201 = new BitSet(new long[]{0x00000000FFFFFFF0L});
    public static final BitSet FOLLOW_END_EXAMPLE_in_exampleParagraph206 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_lineText_in_normalParagraph218 = new BitSet(new long[]{0x0000000000000100L});
    public static final BitSet FOLLOW_NL_in_normalParagraph220 = new BitSet(new long[]{0x00000000FFFFFEF2L});
    public static final BitSet FOLLOW_textItem_in_lineText231 = new BitSet(new long[]{0x00000000FFFFFEF2L});
    public static final BitSet FOLLOW_normalText_in_textItem240 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_set_in_notNL249 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_notNL_in_normalText259 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_header_in_synpred2_OrgMode144 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_exampleParagraph_in_synpred3_OrgMode154 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_normalParagraph_in_synpred4_OrgMode164 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_lineText_in_synpred7_OrgMode218 = new BitSet(new long[]{0x0000000000000100L});
    public static final BitSet FOLLOW_NL_in_synpred7_OrgMode220 = new BitSet(new long[]{0x0000000000000002L});

}