// $ANTLR 3.2 debian-7 OrgMode.g 2012-12-21 23:55:50

import org.antlr.runtime.*;
import java.util.Stack;
import java.util.List;
import java.util.ArrayList;

public class OrgModeLexer extends Lexer {
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
    public static final int ITALIC_INLINE=22;
    public static final int LINK_URL_DESC=24;
    public static final int NL=8;
    public static final int BEGIN_SRC=6;
    public static final int LE=25;

      protected boolean bol = true;
      boolean in_example = false;
      protected boolean assertIsKeyword = true;
      
      void debug(String s) {
          System.out.println(s + " at " + getLine() + ":" + getCharPositionInLine());
      }

      void debug(int do_it, String s) {
          if (do_it != 0) {
              System.out.println(s + " at " + getLine() + ":" + getCharPositionInLine());
          }
      }
      boolean looking_backat_white_space() {
          return true;
      }

      boolean inExample(String w) {
          System.out.println("checking for inExample in " + w);
          return in_example && getCharPositionInLine() == 0;
      }
      boolean looking_at_white_space() {
          return true;
      }

      public String getErrorMessage(RecognitionException e,
                                    String[] tokenNames)
      {
          List stack = getRuleInvocationStack(e, this.getClass().getName());
          String msg = null;
          if ( e instanceof NoViableAltException ) {
              NoViableAltException nvae = (NoViableAltException)e;
              msg = " no viable alt; token="+e.token+
                  " (decision="+nvae.decisionNumber+
                  " state "+nvae.stateNumber+")"+
                  " decision=<<"+nvae.grammarDecisionDescription+">>";
          }
          else {
              msg = super.getErrorMessage(e, tokenNames);
          }
          return stack+" "+msg;
      }


    // delegates
    // delegators

    public OrgModeLexer() {;} 
    public OrgModeLexer(CharStream input) {
        this(input, new RecognizerSharedState());
    }
    public OrgModeLexer(CharStream input, RecognizerSharedState state) {
        super(input,state);

    }
    public String getGrammarFileName() { return "OrgMode.g"; }

    // $ANTLR start "BEG_BLOCK"
    public final void mBEG_BLOCK() throws RecognitionException {
        try {
            int _type = BEG_BLOCK;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // OrgMode.g:101:11: ( '#+begin_' WORDF )
            // OrgMode.g:101:13: '#+begin_' WORDF
            {
            match("#+begin_"); 

            mWORDF(); 

            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "BEG_BLOCK"

    // $ANTLR start "END_BLOCK"
    public final void mEND_BLOCK() throws RecognitionException {
        try {
            int _type = END_BLOCK;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // OrgMode.g:102:11: ( '#+end_' WORDF )
            // OrgMode.g:102:13: '#+end_' WORDF
            {
            match("#+end_"); 

            mWORDF(); 

            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "END_BLOCK"

    // $ANTLR start "WORDF"
    public final void mWORDF() throws RecognitionException {
        try {
            // OrgMode.g:105:7: ( ( 'a' .. 'z' | 'A' .. 'Z' | '_' )+ )
            // OrgMode.g:105:9: ( 'a' .. 'z' | 'A' .. 'Z' | '_' )+
            {
            // OrgMode.g:105:9: ( 'a' .. 'z' | 'A' .. 'Z' | '_' )+
            int cnt1=0;
            loop1:
            do {
                int alt1=2;
                int LA1_0 = input.LA(1);

                if ( ((LA1_0>='A' && LA1_0<='Z')||LA1_0=='_'||(LA1_0>='a' && LA1_0<='z')) ) {
                    alt1=1;
                }


                switch (alt1) {
            	case 1 :
            	    // OrgMode.g:
            	    {
            	    if ( (input.LA(1)>='A' && input.LA(1)<='Z')||input.LA(1)=='_'||(input.LA(1)>='a' && input.LA(1)<='z') ) {
            	        input.consume();

            	    }
            	    else {
            	        MismatchedSetException mse = new MismatchedSetException(null,input);
            	        recover(mse);
            	        throw mse;}


            	    }
            	    break;

            	default :
            	    if ( cnt1 >= 1 ) break loop1;
                        EarlyExitException eee =
                            new EarlyExitException(1, input);
                        throw eee;
                }
                cnt1++;
            } while (true);


            }

        }
        finally {
        }
    }
    // $ANTLR end "WORDF"

    // $ANTLR start "WS"
    public final void mWS() throws RecognitionException {
        try {
            int _type = WS;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // OrgMode.g:107:4: ( ( ' ' | '\\t' | '\\r' | '\\f' )+ )
            // OrgMode.g:107:8: ( ' ' | '\\t' | '\\r' | '\\f' )+
            {
            // OrgMode.g:107:8: ( ' ' | '\\t' | '\\r' | '\\f' )+
            int cnt2=0;
            loop2:
            do {
                int alt2=2;
                int LA2_0 = input.LA(1);

                if ( (LA2_0=='\t'||(LA2_0>='\f' && LA2_0<='\r')||LA2_0==' ') ) {
                    alt2=1;
                }


                switch (alt2) {
            	case 1 :
            	    // OrgMode.g:
            	    {
            	    if ( input.LA(1)=='\t'||(input.LA(1)>='\f' && input.LA(1)<='\r')||input.LA(1)==' ' ) {
            	        input.consume();

            	    }
            	    else {
            	        MismatchedSetException mse = new MismatchedSetException(null,input);
            	        recover(mse);
            	        throw mse;}


            	    }
            	    break;

            	default :
            	    if ( cnt2 >= 1 ) break loop2;
                        EarlyExitException eee =
                            new EarlyExitException(2, input);
                        throw eee;
                }
                cnt2++;
            } while (true);

            _channel = HIDDEN;

            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "WS"

    // $ANTLR start "WSF"
    public final void mWSF() throws RecognitionException {
        try {
            // OrgMode.g:110:5: ( ' ' | '\\t' | '\\f' | '\\r' )
            // OrgMode.g:
            {
            if ( input.LA(1)=='\t'||(input.LA(1)>='\f' && input.LA(1)<='\r')||input.LA(1)==' ' ) {
                input.consume();

            }
            else {
                MismatchedSetException mse = new MismatchedSetException(null,input);
                recover(mse);
                throw mse;}


            }

        }
        finally {
        }
    }
    // $ANTLR end "WSF"

    // $ANTLR start "BOLD_INLINE"
    public final void mBOLD_INLINE() throws RecognitionException {
        try {
            int _type = BOLD_INLINE;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // OrgMode.g:135:13: ( '*' ~ ( '*' | AWSF ) ( . )* (~ AWSF '*' ( WSF | NL ) | NL ) )
            // OrgMode.g:135:15: '*' ~ ( '*' | AWSF ) ( . )* (~ AWSF '*' ( WSF | NL ) | NL )
            {
            match('*'); 
            if ( (input.LA(1)>='\u0000' && input.LA(1)<='\b')||input.LA(1)=='\u000B'||(input.LA(1)>='\u000E' && input.LA(1)<='\u001F')||(input.LA(1)>='!' && input.LA(1)<=')')||(input.LA(1)>='+' && input.LA(1)<='\uFFFF') ) {
                input.consume();

            }
            else {
                MismatchedSetException mse = new MismatchedSetException(null,input);
                recover(mse);
                throw mse;}

            // OrgMode.g:135:33: ( . )*
            loop3:
            do {
                int alt3=2;
                int LA3_0 = input.LA(1);

                if ( ((LA3_0>='\u0000' && LA3_0<='\b')||LA3_0=='\u000B'||(LA3_0>='\u000E' && LA3_0<='\u001F')||(LA3_0>='!' && LA3_0<='\uFFFF')) ) {
                    int LA3_1 = input.LA(2);

                    if ( (LA3_1=='*') ) {
                        int LA3_4 = input.LA(3);

                        if ( ((LA3_4>='\u0000' && LA3_4<='\b')||LA3_4=='\u000B'||(LA3_4>='\u000E' && LA3_4<='\u001F')||(LA3_4>='!' && LA3_4<='\uFFFF')) ) {
                            alt3=1;
                        }
                        else if ( (LA3_4=='\n') ) {
                            alt3=2;
                        }
                        else if ( (LA3_4=='\t'||(LA3_4>='\f' && LA3_4<='\r')||LA3_4==' ') ) {
                            alt3=2;
                        }


                    }
                    else if ( ((LA3_1>='\u0000' && LA3_1<=')')||(LA3_1>='+' && LA3_1<='\uFFFF')) ) {
                        alt3=1;
                    }


                }
                else if ( (LA3_0=='\n') ) {
                    alt3=2;
                }
                else if ( (LA3_0=='\t'||(LA3_0>='\f' && LA3_0<='\r')||LA3_0==' ') ) {
                    alt3=1;
                }


                switch (alt3) {
            	case 1 :
            	    // OrgMode.g:135:33: .
            	    {
            	    matchAny(); 

            	    }
            	    break;

            	default :
            	    break loop3;
                }
            } while (true);

            // OrgMode.g:136:9: (~ AWSF '*' ( WSF | NL ) | NL )
            int alt5=2;
            int LA5_0 = input.LA(1);

            if ( ((LA5_0>='\u0000' && LA5_0<='\b')||LA5_0=='\u000B'||(LA5_0>='\u000E' && LA5_0<='\u001F')||(LA5_0>='!' && LA5_0<='\uFFFF')) ) {
                alt5=1;
            }
            else if ( (LA5_0=='\n') ) {
                alt5=2;
            }
            else {
                NoViableAltException nvae =
                    new NoViableAltException("", 5, 0, input);

                throw nvae;
            }
            switch (alt5) {
                case 1 :
                    // OrgMode.g:137:13: ~ AWSF '*' ( WSF | NL )
                    {
                    if ( (input.LA(1)>='\u0000' && input.LA(1)<='\u000F')||(input.LA(1)>='\u0011' && input.LA(1)<='\uFFFF') ) {
                        input.consume();

                    }
                    else {
                        MismatchedSetException mse = new MismatchedSetException(null,input);
                        recover(mse);
                        throw mse;}

                    match('*'); 
                    // OrgMode.g:137:23: ( WSF | NL )
                    int alt4=2;
                    int LA4_0 = input.LA(1);

                    if ( (LA4_0=='\t'||(LA4_0>='\f' && LA4_0<='\r')||LA4_0==' ') ) {
                        alt4=1;
                    }
                    else if ( (LA4_0=='\n') ) {
                        alt4=2;
                    }
                    else {
                        NoViableAltException nvae =
                            new NoViableAltException("", 4, 0, input);

                        throw nvae;
                    }
                    switch (alt4) {
                        case 1 :
                            // OrgMode.g:138:17: WSF
                            {
                            mWSF(); 
                             emit(new CommonToken(BOLD_INLINE, getText())); debug("matched code BOLD_INLINE :" + getText()); 

                            }
                            break;
                        case 2 :
                            // OrgMode.g:139:17: NL
                            {
                            mNL(); 
                             emit(new CommonToken(BOLD_INLINE, getText())); emit(new CommonToken(NL)); 

                            }
                            break;

                    }


                    }
                    break;
                case 2 :
                    // OrgMode.g:142:11: NL
                    {
                    mNL(); 
                     emit(new CommonToken(NWS, getText())); emit(new CommonToken(NL)); 

                    }
                    break;

            }


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "BOLD_INLINE"

    // $ANTLR start "UNDERLINED_INLINE"
    public final void mUNDERLINED_INLINE() throws RecognitionException {
        try {
            int _type = UNDERLINED_INLINE;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // OrgMode.g:145:19: ( '_' ~ ( '_' | AWSF ) ( . )* (~ AWSF '_' ( WSF | NL ) | NL ) )
            // OrgMode.g:145:21: '_' ~ ( '_' | AWSF ) ( . )* (~ AWSF '_' ( WSF | NL ) | NL )
            {
            match('_'); 
            if ( (input.LA(1)>='\u0000' && input.LA(1)<='\b')||input.LA(1)=='\u000B'||(input.LA(1)>='\u000E' && input.LA(1)<='\u001F')||(input.LA(1)>='!' && input.LA(1)<='^')||(input.LA(1)>='`' && input.LA(1)<='\uFFFF') ) {
                input.consume();

            }
            else {
                MismatchedSetException mse = new MismatchedSetException(null,input);
                recover(mse);
                throw mse;}

            // OrgMode.g:145:39: ( . )*
            loop6:
            do {
                int alt6=2;
                int LA6_0 = input.LA(1);

                if ( ((LA6_0>='\u0000' && LA6_0<='\b')||LA6_0=='\u000B'||(LA6_0>='\u000E' && LA6_0<='\u001F')||(LA6_0>='!' && LA6_0<='\uFFFF')) ) {
                    int LA6_1 = input.LA(2);

                    if ( (LA6_1=='_') ) {
                        int LA6_4 = input.LA(3);

                        if ( ((LA6_4>='\u0000' && LA6_4<='\b')||LA6_4=='\u000B'||(LA6_4>='\u000E' && LA6_4<='\u001F')||(LA6_4>='!' && LA6_4<='\uFFFF')) ) {
                            alt6=1;
                        }
                        else if ( (LA6_4=='\t'||(LA6_4>='\f' && LA6_4<='\r')||LA6_4==' ') ) {
                            alt6=2;
                        }
                        else if ( (LA6_4=='\n') ) {
                            alt6=2;
                        }


                    }
                    else if ( ((LA6_1>='\u0000' && LA6_1<='^')||(LA6_1>='`' && LA6_1<='\uFFFF')) ) {
                        alt6=1;
                    }


                }
                else if ( (LA6_0=='\n') ) {
                    alt6=2;
                }
                else if ( (LA6_0=='\t'||(LA6_0>='\f' && LA6_0<='\r')||LA6_0==' ') ) {
                    alt6=1;
                }


                switch (alt6) {
            	case 1 :
            	    // OrgMode.g:145:39: .
            	    {
            	    matchAny(); 

            	    }
            	    break;

            	default :
            	    break loop6;
                }
            } while (true);

            // OrgMode.g:146:9: (~ AWSF '_' ( WSF | NL ) | NL )
            int alt8=2;
            int LA8_0 = input.LA(1);

            if ( ((LA8_0>='\u0000' && LA8_0<='\b')||LA8_0=='\u000B'||(LA8_0>='\u000E' && LA8_0<='\u001F')||(LA8_0>='!' && LA8_0<='\uFFFF')) ) {
                alt8=1;
            }
            else if ( (LA8_0=='\n') ) {
                alt8=2;
            }
            else {
                NoViableAltException nvae =
                    new NoViableAltException("", 8, 0, input);

                throw nvae;
            }
            switch (alt8) {
                case 1 :
                    // OrgMode.g:147:13: ~ AWSF '_' ( WSF | NL )
                    {
                    if ( (input.LA(1)>='\u0000' && input.LA(1)<='\u000F')||(input.LA(1)>='\u0011' && input.LA(1)<='\uFFFF') ) {
                        input.consume();

                    }
                    else {
                        MismatchedSetException mse = new MismatchedSetException(null,input);
                        recover(mse);
                        throw mse;}

                    match('_'); 
                    // OrgMode.g:147:23: ( WSF | NL )
                    int alt7=2;
                    int LA7_0 = input.LA(1);

                    if ( (LA7_0=='\t'||(LA7_0>='\f' && LA7_0<='\r')||LA7_0==' ') ) {
                        alt7=1;
                    }
                    else if ( (LA7_0=='\n') ) {
                        alt7=2;
                    }
                    else {
                        NoViableAltException nvae =
                            new NoViableAltException("", 7, 0, input);

                        throw nvae;
                    }
                    switch (alt7) {
                        case 1 :
                            // OrgMode.g:148:17: WSF
                            {
                            mWSF(); 
                             emit(new CommonToken(UNDERLINED_INLINE, getText())); debug("matched code UNDERLINED_INLINE :" + getText()); 

                            }
                            break;
                        case 2 :
                            // OrgMode.g:149:17: NL
                            {
                            mNL(); 
                             emit(new CommonToken(UNDERLINED_INLINE, getText())); emit(new CommonToken(NL)); 

                            }
                            break;

                    }


                    }
                    break;
                case 2 :
                    // OrgMode.g:152:11: NL
                    {
                    mNL(); 
                     emit(new CommonToken(NWS, getText())); emit(new CommonToken(NL)); 

                    }
                    break;

            }


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "UNDERLINED_INLINE"

    // $ANTLR start "CODE_INLINE"
    public final void mCODE_INLINE() throws RecognitionException {
        try {
            int _type = CODE_INLINE;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // OrgMode.g:155:13: ( '=' ~ ( '=' | AWSF ) ( . )* (~ AWSF '=' ( WSF | NL ) | NL ) )
            // OrgMode.g:155:15: '=' ~ ( '=' | AWSF ) ( . )* (~ AWSF '=' ( WSF | NL ) | NL )
            {
            match('='); 
            if ( (input.LA(1)>='\u0000' && input.LA(1)<='\b')||input.LA(1)=='\u000B'||(input.LA(1)>='\u000E' && input.LA(1)<='\u001F')||(input.LA(1)>='!' && input.LA(1)<='<')||(input.LA(1)>='>' && input.LA(1)<='\uFFFF') ) {
                input.consume();

            }
            else {
                MismatchedSetException mse = new MismatchedSetException(null,input);
                recover(mse);
                throw mse;}

            // OrgMode.g:155:33: ( . )*
            loop9:
            do {
                int alt9=2;
                int LA9_0 = input.LA(1);

                if ( ((LA9_0>='\u0000' && LA9_0<='\b')||LA9_0=='\u000B'||(LA9_0>='\u000E' && LA9_0<='\u001F')||(LA9_0>='!' && LA9_0<='\uFFFF')) ) {
                    int LA9_1 = input.LA(2);

                    if ( (LA9_1=='=') ) {
                        int LA9_4 = input.LA(3);

                        if ( ((LA9_4>='\u0000' && LA9_4<='\b')||LA9_4=='\u000B'||(LA9_4>='\u000E' && LA9_4<='\u001F')||(LA9_4>='!' && LA9_4<='\uFFFF')) ) {
                            alt9=1;
                        }
                        else if ( (LA9_4=='\t'||(LA9_4>='\f' && LA9_4<='\r')||LA9_4==' ') ) {
                            alt9=2;
                        }
                        else if ( (LA9_4=='\n') ) {
                            alt9=2;
                        }


                    }
                    else if ( ((LA9_1>='\u0000' && LA9_1<='<')||(LA9_1>='>' && LA9_1<='\uFFFF')) ) {
                        alt9=1;
                    }


                }
                else if ( (LA9_0=='\n') ) {
                    alt9=2;
                }
                else if ( (LA9_0=='\t'||(LA9_0>='\f' && LA9_0<='\r')||LA9_0==' ') ) {
                    alt9=1;
                }


                switch (alt9) {
            	case 1 :
            	    // OrgMode.g:155:33: .
            	    {
            	    matchAny(); 

            	    }
            	    break;

            	default :
            	    break loop9;
                }
            } while (true);

            // OrgMode.g:156:9: (~ AWSF '=' ( WSF | NL ) | NL )
            int alt11=2;
            int LA11_0 = input.LA(1);

            if ( ((LA11_0>='\u0000' && LA11_0<='\b')||LA11_0=='\u000B'||(LA11_0>='\u000E' && LA11_0<='\u001F')||(LA11_0>='!' && LA11_0<='\uFFFF')) ) {
                alt11=1;
            }
            else if ( (LA11_0=='\n') ) {
                alt11=2;
            }
            else {
                NoViableAltException nvae =
                    new NoViableAltException("", 11, 0, input);

                throw nvae;
            }
            switch (alt11) {
                case 1 :
                    // OrgMode.g:157:13: ~ AWSF '=' ( WSF | NL )
                    {
                    if ( (input.LA(1)>='\u0000' && input.LA(1)<='\u000F')||(input.LA(1)>='\u0011' && input.LA(1)<='\uFFFF') ) {
                        input.consume();

                    }
                    else {
                        MismatchedSetException mse = new MismatchedSetException(null,input);
                        recover(mse);
                        throw mse;}

                    match('='); 
                    // OrgMode.g:157:23: ( WSF | NL )
                    int alt10=2;
                    int LA10_0 = input.LA(1);

                    if ( (LA10_0=='\t'||(LA10_0>='\f' && LA10_0<='\r')||LA10_0==' ') ) {
                        alt10=1;
                    }
                    else if ( (LA10_0=='\n') ) {
                        alt10=2;
                    }
                    else {
                        NoViableAltException nvae =
                            new NoViableAltException("", 10, 0, input);

                        throw nvae;
                    }
                    switch (alt10) {
                        case 1 :
                            // OrgMode.g:158:17: WSF
                            {
                            mWSF(); 
                             emit(new CommonToken(CODE_INLINE, getText())); debug("matched code CODE_INLINE :" + getText()); 

                            }
                            break;
                        case 2 :
                            // OrgMode.g:159:17: NL
                            {
                            mNL(); 
                             emit(new CommonToken(CODE_INLINE, getText())); emit(new CommonToken(NL)); 

                            }
                            break;

                    }


                    }
                    break;
                case 2 :
                    // OrgMode.g:162:11: NL
                    {
                    mNL(); 
                     emit(new CommonToken(NWS, getText())); emit(new CommonToken(NL)); 

                    }
                    break;

            }


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "CODE_INLINE"

    // $ANTLR start "VERBATIM_INLINE"
    public final void mVERBATIM_INLINE() throws RecognitionException {
        try {
            int _type = VERBATIM_INLINE;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // OrgMode.g:165:17: ( '~' ~ ( '~' | AWSF ) ( . )* (~ AWSF '~' ( WSF | NL ) | NL ) )
            // OrgMode.g:165:19: '~' ~ ( '~' | AWSF ) ( . )* (~ AWSF '~' ( WSF | NL ) | NL )
            {
            match('~'); 
            if ( (input.LA(1)>='\u0000' && input.LA(1)<='\b')||input.LA(1)=='\u000B'||(input.LA(1)>='\u000E' && input.LA(1)<='\u001F')||(input.LA(1)>='!' && input.LA(1)<='}')||(input.LA(1)>='\u007F' && input.LA(1)<='\uFFFF') ) {
                input.consume();

            }
            else {
                MismatchedSetException mse = new MismatchedSetException(null,input);
                recover(mse);
                throw mse;}

            // OrgMode.g:165:37: ( . )*
            loop12:
            do {
                int alt12=2;
                int LA12_0 = input.LA(1);

                if ( ((LA12_0>='\u0000' && LA12_0<='\b')||LA12_0=='\u000B'||(LA12_0>='\u000E' && LA12_0<='\u001F')||(LA12_0>='!' && LA12_0<='\uFFFF')) ) {
                    int LA12_1 = input.LA(2);

                    if ( (LA12_1=='~') ) {
                        int LA12_4 = input.LA(3);

                        if ( ((LA12_4>='\u0000' && LA12_4<='\b')||LA12_4=='\u000B'||(LA12_4>='\u000E' && LA12_4<='\u001F')||(LA12_4>='!' && LA12_4<='\uFFFF')) ) {
                            alt12=1;
                        }
                        else if ( (LA12_4=='\n') ) {
                            alt12=2;
                        }
                        else if ( (LA12_4=='\t'||(LA12_4>='\f' && LA12_4<='\r')||LA12_4==' ') ) {
                            alt12=2;
                        }


                    }
                    else if ( ((LA12_1>='\u0000' && LA12_1<='}')||(LA12_1>='\u007F' && LA12_1<='\uFFFF')) ) {
                        alt12=1;
                    }


                }
                else if ( (LA12_0=='\n') ) {
                    alt12=2;
                }
                else if ( (LA12_0=='\t'||(LA12_0>='\f' && LA12_0<='\r')||LA12_0==' ') ) {
                    alt12=1;
                }


                switch (alt12) {
            	case 1 :
            	    // OrgMode.g:165:37: .
            	    {
            	    matchAny(); 

            	    }
            	    break;

            	default :
            	    break loop12;
                }
            } while (true);

            // OrgMode.g:166:9: (~ AWSF '~' ( WSF | NL ) | NL )
            int alt14=2;
            int LA14_0 = input.LA(1);

            if ( ((LA14_0>='\u0000' && LA14_0<='\b')||LA14_0=='\u000B'||(LA14_0>='\u000E' && LA14_0<='\u001F')||(LA14_0>='!' && LA14_0<='\uFFFF')) ) {
                alt14=1;
            }
            else if ( (LA14_0=='\n') ) {
                alt14=2;
            }
            else {
                NoViableAltException nvae =
                    new NoViableAltException("", 14, 0, input);

                throw nvae;
            }
            switch (alt14) {
                case 1 :
                    // OrgMode.g:167:13: ~ AWSF '~' ( WSF | NL )
                    {
                    if ( (input.LA(1)>='\u0000' && input.LA(1)<='\u000F')||(input.LA(1)>='\u0011' && input.LA(1)<='\uFFFF') ) {
                        input.consume();

                    }
                    else {
                        MismatchedSetException mse = new MismatchedSetException(null,input);
                        recover(mse);
                        throw mse;}

                    match('~'); 
                    // OrgMode.g:167:23: ( WSF | NL )
                    int alt13=2;
                    int LA13_0 = input.LA(1);

                    if ( (LA13_0=='\t'||(LA13_0>='\f' && LA13_0<='\r')||LA13_0==' ') ) {
                        alt13=1;
                    }
                    else if ( (LA13_0=='\n') ) {
                        alt13=2;
                    }
                    else {
                        NoViableAltException nvae =
                            new NoViableAltException("", 13, 0, input);

                        throw nvae;
                    }
                    switch (alt13) {
                        case 1 :
                            // OrgMode.g:168:17: WSF
                            {
                            mWSF(); 
                             emit(new CommonToken(VERBATIM_INLINE, getText())); debug("matched code VERBATIM_INLINE :" + getText()); 

                            }
                            break;
                        case 2 :
                            // OrgMode.g:169:17: NL
                            {
                            mNL(); 
                             emit(new CommonToken(VERBATIM_INLINE, getText())); emit(new CommonToken(NL)); 

                            }
                            break;

                    }


                    }
                    break;
                case 2 :
                    // OrgMode.g:172:11: NL
                    {
                    mNL(); 
                     emit(new CommonToken(NWS, getText())); emit(new CommonToken(NL)); 

                    }
                    break;

            }


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "VERBATIM_INLINE"

    // $ANTLR start "STRIKE_INLINE"
    public final void mSTRIKE_INLINE() throws RecognitionException {
        try {
            int _type = STRIKE_INLINE;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // OrgMode.g:175:15: ( '+' ~ ( '+' | AWSF ) ( . )* (~ AWSF '+' ( WSF | NL ) | NL ) )
            // OrgMode.g:175:17: '+' ~ ( '+' | AWSF ) ( . )* (~ AWSF '+' ( WSF | NL ) | NL )
            {
            match('+'); 
            if ( (input.LA(1)>='\u0000' && input.LA(1)<='\b')||input.LA(1)=='\u000B'||(input.LA(1)>='\u000E' && input.LA(1)<='\u001F')||(input.LA(1)>='!' && input.LA(1)<='*')||(input.LA(1)>=',' && input.LA(1)<='\uFFFF') ) {
                input.consume();

            }
            else {
                MismatchedSetException mse = new MismatchedSetException(null,input);
                recover(mse);
                throw mse;}

            // OrgMode.g:175:35: ( . )*
            loop15:
            do {
                int alt15=2;
                int LA15_0 = input.LA(1);

                if ( ((LA15_0>='\u0000' && LA15_0<='\b')||LA15_0=='\u000B'||(LA15_0>='\u000E' && LA15_0<='\u001F')||(LA15_0>='!' && LA15_0<='\uFFFF')) ) {
                    int LA15_1 = input.LA(2);

                    if ( (LA15_1=='+') ) {
                        int LA15_4 = input.LA(3);

                        if ( ((LA15_4>='\u0000' && LA15_4<='\b')||LA15_4=='\u000B'||(LA15_4>='\u000E' && LA15_4<='\u001F')||(LA15_4>='!' && LA15_4<='\uFFFF')) ) {
                            alt15=1;
                        }
                        else if ( (LA15_4=='\n') ) {
                            alt15=2;
                        }
                        else if ( (LA15_4=='\t'||(LA15_4>='\f' && LA15_4<='\r')||LA15_4==' ') ) {
                            alt15=2;
                        }


                    }
                    else if ( ((LA15_1>='\u0000' && LA15_1<='*')||(LA15_1>=',' && LA15_1<='\uFFFF')) ) {
                        alt15=1;
                    }


                }
                else if ( (LA15_0=='\n') ) {
                    alt15=2;
                }
                else if ( (LA15_0=='\t'||(LA15_0>='\f' && LA15_0<='\r')||LA15_0==' ') ) {
                    alt15=1;
                }


                switch (alt15) {
            	case 1 :
            	    // OrgMode.g:175:35: .
            	    {
            	    matchAny(); 

            	    }
            	    break;

            	default :
            	    break loop15;
                }
            } while (true);

            // OrgMode.g:176:5: (~ AWSF '+' ( WSF | NL ) | NL )
            int alt17=2;
            int LA17_0 = input.LA(1);

            if ( ((LA17_0>='\u0000' && LA17_0<='\b')||LA17_0=='\u000B'||(LA17_0>='\u000E' && LA17_0<='\u001F')||(LA17_0>='!' && LA17_0<='\uFFFF')) ) {
                alt17=1;
            }
            else if ( (LA17_0=='\n') ) {
                alt17=2;
            }
            else {
                NoViableAltException nvae =
                    new NoViableAltException("", 17, 0, input);

                throw nvae;
            }
            switch (alt17) {
                case 1 :
                    // OrgMode.g:177:9: ~ AWSF '+' ( WSF | NL )
                    {
                    if ( (input.LA(1)>='\u0000' && input.LA(1)<='\u000F')||(input.LA(1)>='\u0011' && input.LA(1)<='\uFFFF') ) {
                        input.consume();

                    }
                    else {
                        MismatchedSetException mse = new MismatchedSetException(null,input);
                        recover(mse);
                        throw mse;}

                    match('+'); 
                    // OrgMode.g:177:19: ( WSF | NL )
                    int alt16=2;
                    int LA16_0 = input.LA(1);

                    if ( (LA16_0=='\t'||(LA16_0>='\f' && LA16_0<='\r')||LA16_0==' ') ) {
                        alt16=1;
                    }
                    else if ( (LA16_0=='\n') ) {
                        alt16=2;
                    }
                    else {
                        NoViableAltException nvae =
                            new NoViableAltException("", 16, 0, input);

                        throw nvae;
                    }
                    switch (alt16) {
                        case 1 :
                            // OrgMode.g:178:13: WSF
                            {
                            mWSF(); 
                             emit(new CommonToken(STRIKE_INLINE, getText())); debug("matched code STRIKE_INLINE :" + getText()); 

                            }
                            break;
                        case 2 :
                            // OrgMode.g:179:9: NL
                            {
                            mNL(); 
                             emit(new CommonToken(STRIKE_INLINE, getText())); emit(new CommonToken(NL)); 

                            }
                            break;

                    }


                    }
                    break;
                case 2 :
                    // OrgMode.g:182:3: NL
                    {
                    mNL(); 
                     emit(new CommonToken(NWS, getText())); emit(new CommonToken(NL)); 

                    }
                    break;

            }


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "STRIKE_INLINE"

    // $ANTLR start "ITALIC_INLINE"
    public final void mITALIC_INLINE() throws RecognitionException {
        try {
            int _type = ITALIC_INLINE;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // OrgMode.g:185:15: ( '/' ~ ( '/' | AWSF ) ( . )* (~ AWSF '/' ( WSF | NL ) | NL ) )
            // OrgMode.g:185:17: '/' ~ ( '/' | AWSF ) ( . )* (~ AWSF '/' ( WSF | NL ) | NL )
            {
            match('/'); 
            if ( (input.LA(1)>='\u0000' && input.LA(1)<='\b')||input.LA(1)=='\u000B'||(input.LA(1)>='\u000E' && input.LA(1)<='\u001F')||(input.LA(1)>='!' && input.LA(1)<='.')||(input.LA(1)>='0' && input.LA(1)<='\uFFFF') ) {
                input.consume();

            }
            else {
                MismatchedSetException mse = new MismatchedSetException(null,input);
                recover(mse);
                throw mse;}

            // OrgMode.g:185:35: ( . )*
            loop18:
            do {
                int alt18=2;
                int LA18_0 = input.LA(1);

                if ( ((LA18_0>='\u0000' && LA18_0<='\b')||LA18_0=='\u000B'||(LA18_0>='\u000E' && LA18_0<='\u001F')||(LA18_0>='!' && LA18_0<='\uFFFF')) ) {
                    int LA18_1 = input.LA(2);

                    if ( (LA18_1=='/') ) {
                        int LA18_4 = input.LA(3);

                        if ( ((LA18_4>='\u0000' && LA18_4<='\b')||LA18_4=='\u000B'||(LA18_4>='\u000E' && LA18_4<='\u001F')||(LA18_4>='!' && LA18_4<='\uFFFF')) ) {
                            alt18=1;
                        }
                        else if ( (LA18_4=='\t'||(LA18_4>='\f' && LA18_4<='\r')||LA18_4==' ') ) {
                            alt18=2;
                        }
                        else if ( (LA18_4=='\n') ) {
                            alt18=2;
                        }


                    }
                    else if ( ((LA18_1>='\u0000' && LA18_1<='.')||(LA18_1>='0' && LA18_1<='\uFFFF')) ) {
                        alt18=1;
                    }


                }
                else if ( (LA18_0=='\n') ) {
                    alt18=2;
                }
                else if ( (LA18_0=='\t'||(LA18_0>='\f' && LA18_0<='\r')||LA18_0==' ') ) {
                    alt18=1;
                }


                switch (alt18) {
            	case 1 :
            	    // OrgMode.g:185:35: .
            	    {
            	    matchAny(); 

            	    }
            	    break;

            	default :
            	    break loop18;
                }
            } while (true);

            // OrgMode.g:186:5: (~ AWSF '/' ( WSF | NL ) | NL )
            int alt20=2;
            int LA20_0 = input.LA(1);

            if ( ((LA20_0>='\u0000' && LA20_0<='\b')||LA20_0=='\u000B'||(LA20_0>='\u000E' && LA20_0<='\u001F')||(LA20_0>='!' && LA20_0<='\uFFFF')) ) {
                alt20=1;
            }
            else if ( (LA20_0=='\n') ) {
                alt20=2;
            }
            else {
                NoViableAltException nvae =
                    new NoViableAltException("", 20, 0, input);

                throw nvae;
            }
            switch (alt20) {
                case 1 :
                    // OrgMode.g:187:9: ~ AWSF '/' ( WSF | NL )
                    {
                    if ( (input.LA(1)>='\u0000' && input.LA(1)<='\u000F')||(input.LA(1)>='\u0011' && input.LA(1)<='\uFFFF') ) {
                        input.consume();

                    }
                    else {
                        MismatchedSetException mse = new MismatchedSetException(null,input);
                        recover(mse);
                        throw mse;}

                    match('/'); 
                    // OrgMode.g:187:19: ( WSF | NL )
                    int alt19=2;
                    int LA19_0 = input.LA(1);

                    if ( (LA19_0=='\t'||(LA19_0>='\f' && LA19_0<='\r')||LA19_0==' ') ) {
                        alt19=1;
                    }
                    else if ( (LA19_0=='\n') ) {
                        alt19=2;
                    }
                    else {
                        NoViableAltException nvae =
                            new NoViableAltException("", 19, 0, input);

                        throw nvae;
                    }
                    switch (alt19) {
                        case 1 :
                            // OrgMode.g:188:13: WSF
                            {
                            mWSF(); 
                             emit(new CommonToken(ITALIC_INLINE, getText())); debug("matched code ITALIC_INLINE :" + getText()); 

                            }
                            break;
                        case 2 :
                            // OrgMode.g:189:9: NL
                            {
                            mNL(); 
                             emit(new CommonToken(ITALIC_INLINE, getText())); emit(new CommonToken(NL)); 

                            }
                            break;

                    }


                    }
                    break;
                case 2 :
                    // OrgMode.g:192:3: NL
                    {
                    mNL(); 
                     emit(new CommonToken(NWS, getText())); emit(new CommonToken(NL)); 

                    }
                    break;

            }


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "ITALIC_INLINE"

    // $ANTLR start "LINK_URL"
    public final void mLINK_URL() throws RecognitionException {
        try {
            int _type = LINK_URL;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // OrgMode.g:197:10: ( '[[' (~ ( '[' | ']' ) )* ']]' )
            // OrgMode.g:197:12: '[[' (~ ( '[' | ']' ) )* ']]'
            {
            match("[["); 

            // OrgMode.g:197:17: (~ ( '[' | ']' ) )*
            loop21:
            do {
                int alt21=2;
                int LA21_0 = input.LA(1);

                if ( ((LA21_0>='\u0000' && LA21_0<='Z')||LA21_0=='\\'||(LA21_0>='^' && LA21_0<='\uFFFF')) ) {
                    alt21=1;
                }


                switch (alt21) {
            	case 1 :
            	    // OrgMode.g:197:17: ~ ( '[' | ']' )
            	    {
            	    if ( (input.LA(1)>='\u0000' && input.LA(1)<='Z')||input.LA(1)=='\\'||(input.LA(1)>='^' && input.LA(1)<='\uFFFF') ) {
            	        input.consume();

            	    }
            	    else {
            	        MismatchedSetException mse = new MismatchedSetException(null,input);
            	        recover(mse);
            	        throw mse;}


            	    }
            	    break;

            	default :
            	    break loop21;
                }
            } while (true);

            match("]]"); 


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "LINK_URL"

    // $ANTLR start "LINK_URL_DESC"
    public final void mLINK_URL_DESC() throws RecognitionException {
        try {
            int _type = LINK_URL_DESC;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // OrgMode.g:198:15: ( '[[' (~ ( '[' | ']' ) )* '][' (~ ( '[' | ']' ) )* ']]' )
            // OrgMode.g:198:17: '[[' (~ ( '[' | ']' ) )* '][' (~ ( '[' | ']' ) )* ']]'
            {
            match("[["); 

            // OrgMode.g:198:22: (~ ( '[' | ']' ) )*
            loop22:
            do {
                int alt22=2;
                int LA22_0 = input.LA(1);

                if ( ((LA22_0>='\u0000' && LA22_0<='Z')||LA22_0=='\\'||(LA22_0>='^' && LA22_0<='\uFFFF')) ) {
                    alt22=1;
                }


                switch (alt22) {
            	case 1 :
            	    // OrgMode.g:198:22: ~ ( '[' | ']' )
            	    {
            	    if ( (input.LA(1)>='\u0000' && input.LA(1)<='Z')||input.LA(1)=='\\'||(input.LA(1)>='^' && input.LA(1)<='\uFFFF') ) {
            	        input.consume();

            	    }
            	    else {
            	        MismatchedSetException mse = new MismatchedSetException(null,input);
            	        recover(mse);
            	        throw mse;}


            	    }
            	    break;

            	default :
            	    break loop22;
                }
            } while (true);

            match("]["); 

            // OrgMode.g:198:39: (~ ( '[' | ']' ) )*
            loop23:
            do {
                int alt23=2;
                int LA23_0 = input.LA(1);

                if ( ((LA23_0>='\u0000' && LA23_0<='Z')||LA23_0=='\\'||(LA23_0>='^' && LA23_0<='\uFFFF')) ) {
                    alt23=1;
                }


                switch (alt23) {
            	case 1 :
            	    // OrgMode.g:198:39: ~ ( '[' | ']' )
            	    {
            	    if ( (input.LA(1)>='\u0000' && input.LA(1)<='Z')||input.LA(1)=='\\'||(input.LA(1)>='^' && input.LA(1)<='\uFFFF') ) {
            	        input.consume();

            	    }
            	    else {
            	        MismatchedSetException mse = new MismatchedSetException(null,input);
            	        recover(mse);
            	        throw mse;}


            	    }
            	    break;

            	default :
            	    break loop23;
                }
            } while (true);

            match("]]"); 


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "LINK_URL_DESC"

    // $ANTLR start "AWSF"
    public final void mAWSF() throws RecognitionException {
        try {
            // OrgMode.g:200:6: ( WSF | NL )
            // OrgMode.g:
            {
            if ( (input.LA(1)>='\t' && input.LA(1)<='\n')||(input.LA(1)>='\f' && input.LA(1)<='\r')||input.LA(1)==' ' ) {
                input.consume();

            }
            else {
                MismatchedSetException mse = new MismatchedSetException(null,input);
                recover(mse);
                throw mse;}


            }

        }
        finally {
        }
    }
    // $ANTLR end "AWSF"

    // $ANTLR start "NL"
    public final void mNL() throws RecognitionException {
        try {
            // OrgMode.g:203:4: ( '\\n' )
            // OrgMode.g:203:6: '\\n'
            {
            match('\n'); 

            }

        }
        finally {
        }
    }
    // $ANTLR end "NL"

    // $ANTLR start "LE"
    public final void mLE() throws RecognitionException {
        try {
            int _type = LE;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // OrgMode.g:204:4: ( '\\n' )
            // OrgMode.g:204:6: '\\n'
            {
            match('\n'); 

            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "LE"

    // $ANTLR start "HEADER_STAR"
    public final void mHEADER_STAR() throws RecognitionException {
        try {
            int _type = HEADER_STAR;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            int s;

            // OrgMode.g:206:13: (s= '*' ( '*' )* WSF )
            // OrgMode.g:206:15: s= '*' ( '*' )* WSF
            {
            s = input.LA(1);
            match('*'); 
            // OrgMode.g:206:21: ( '*' )*
            loop24:
            do {
                int alt24=2;
                int LA24_0 = input.LA(1);

                if ( (LA24_0=='*') ) {
                    alt24=1;
                }


                switch (alt24) {
            	case 1 :
            	    // OrgMode.g:206:21: '*'
            	    {
            	    match('*'); 

            	    }
            	    break;

            	default :
            	    break loop24;
                }
            } while (true);

            mWSF(); 

            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "HEADER_STAR"

    // $ANTLR start "OL_START"
    public final void mOL_START() throws RecognitionException {
        try {
            int _type = OL_START;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // OrgMode.g:207:10: ( ( '0' .. '9' )+ '.' WSF )
            // OrgMode.g:207:12: ( '0' .. '9' )+ '.' WSF
            {
            // OrgMode.g:207:12: ( '0' .. '9' )+
            int cnt25=0;
            loop25:
            do {
                int alt25=2;
                int LA25_0 = input.LA(1);

                if ( ((LA25_0>='0' && LA25_0<='9')) ) {
                    alt25=1;
                }


                switch (alt25) {
            	case 1 :
            	    // OrgMode.g:207:13: '0' .. '9'
            	    {
            	    matchRange('0','9'); 

            	    }
            	    break;

            	default :
            	    if ( cnt25 >= 1 ) break loop25;
                        EarlyExitException eee =
                            new EarlyExitException(25, input);
                        throw eee;
                }
                cnt25++;
            } while (true);

            match('.'); 
            mWSF(); 

            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "OL_START"

    // $ANTLR start "UL_START"
    public final void mUL_START() throws RecognitionException {
        try {
            int _type = UL_START;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // OrgMode.g:208:10: ( ( '-' | '+' | '*' ) WSF )
            // OrgMode.g:208:12: ( '-' | '+' | '*' ) WSF
            {
            if ( (input.LA(1)>='*' && input.LA(1)<='+')||input.LA(1)=='-' ) {
                input.consume();

            }
            else {
                MismatchedSetException mse = new MismatchedSetException(null,input);
                recover(mse);
                throw mse;}

            mWSF(); 

            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "UL_START"

    // $ANTLR start "COL_START"
    public final void mCOL_START() throws RecognitionException {
        try {
            int _type = COL_START;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // OrgMode.g:209:11: ( ':' WSF )
            // OrgMode.g:209:13: ':' WSF
            {
            match(':'); 
            mWSF(); 

            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "COL_START"

    // $ANTLR start "SHARP_SETTING"
    public final void mSHARP_SETTING() throws RecognitionException {
        try {
            int _type = SHARP_SETTING;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // OrgMode.g:210:15: ( '#+' WORDF ':' WSF )
            // OrgMode.g:210:17: '#+' WORDF ':' WSF
            {
            match("#+"); 

            mWORDF(); 
            match(':'); 
            mWSF(); 

            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "SHARP_SETTING"

    // $ANTLR start "WORD_NWS"
    public final void mWORD_NWS() throws RecognitionException {
        try {
            int _type = WORD_NWS;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // OrgMode.g:212:10: ( WORDF (~ AWSF )* )
            // OrgMode.g:212:12: WORDF (~ AWSF )*
            {
            mWORDF(); 
            // OrgMode.g:212:18: (~ AWSF )*
            loop26:
            do {
                int alt26=2;
                int LA26_0 = input.LA(1);

                if ( ((LA26_0>='\u0000' && LA26_0<='\b')||LA26_0=='\u000B'||(LA26_0>='\u000E' && LA26_0<='\u001F')||(LA26_0>='!' && LA26_0<='\uFFFF')) ) {
                    alt26=1;
                }


                switch (alt26) {
            	case 1 :
            	    // OrgMode.g:212:19: ~ AWSF
            	    {
            	    if ( (input.LA(1)>='\u0000' && input.LA(1)<='\u000F')||(input.LA(1)>='\u0011' && input.LA(1)<='\uFFFF') ) {
            	        input.consume();

            	    }
            	    else {
            	        MismatchedSetException mse = new MismatchedSetException(null,input);
            	        recover(mse);
            	        throw mse;}


            	    }
            	    break;

            	default :
            	    break loop26;
                }
            } while (true);


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "WORD_NWS"

    // $ANTLR start "NWS"
    public final void mNWS() throws RecognitionException {
        try {
            int _type = NWS;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // OrgMode.g:213:5: ( (~ AWSF )+ )
            // OrgMode.g:213:7: (~ AWSF )+
            {
            // OrgMode.g:213:7: (~ AWSF )+
            int cnt27=0;
            loop27:
            do {
                int alt27=2;
                int LA27_0 = input.LA(1);

                if ( ((LA27_0>='\u0000' && LA27_0<='\b')||LA27_0=='\u000B'||(LA27_0>='\u000E' && LA27_0<='\u001F')||(LA27_0>='!' && LA27_0<='\uFFFF')) ) {
                    alt27=1;
                }


                switch (alt27) {
            	case 1 :
            	    // OrgMode.g:213:8: ~ AWSF
            	    {
            	    if ( (input.LA(1)>='\u0000' && input.LA(1)<='\u000F')||(input.LA(1)>='\u0011' && input.LA(1)<='\uFFFF') ) {
            	        input.consume();

            	    }
            	    else {
            	        MismatchedSetException mse = new MismatchedSetException(null,input);
            	        recover(mse);
            	        throw mse;}


            	    }
            	    break;

            	default :
            	    if ( cnt27 >= 1 ) break loop27;
                        EarlyExitException eee =
                            new EarlyExitException(27, input);
                        throw eee;
                }
                cnt27++;
            } while (true);

            debug("mateched nws " + getText());

            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "NWS"

    public void mTokens() throws RecognitionException {
        // OrgMode.g:1:8: ( BEG_BLOCK | END_BLOCK | WS | BOLD_INLINE | UNDERLINED_INLINE | CODE_INLINE | VERBATIM_INLINE | STRIKE_INLINE | ITALIC_INLINE | LINK_URL | LINK_URL_DESC | LE | HEADER_STAR | OL_START | UL_START | COL_START | SHARP_SETTING | WORD_NWS | NWS )
        int alt28=19;
        alt28 = dfa28.predict(input);
        switch (alt28) {
            case 1 :
                // OrgMode.g:1:10: BEG_BLOCK
                {
                mBEG_BLOCK(); 

                }
                break;
            case 2 :
                // OrgMode.g:1:20: END_BLOCK
                {
                mEND_BLOCK(); 

                }
                break;
            case 3 :
                // OrgMode.g:1:30: WS
                {
                mWS(); 

                }
                break;
            case 4 :
                // OrgMode.g:1:33: BOLD_INLINE
                {
                mBOLD_INLINE(); 

                }
                break;
            case 5 :
                // OrgMode.g:1:45: UNDERLINED_INLINE
                {
                mUNDERLINED_INLINE(); 

                }
                break;
            case 6 :
                // OrgMode.g:1:63: CODE_INLINE
                {
                mCODE_INLINE(); 

                }
                break;
            case 7 :
                // OrgMode.g:1:75: VERBATIM_INLINE
                {
                mVERBATIM_INLINE(); 

                }
                break;
            case 8 :
                // OrgMode.g:1:91: STRIKE_INLINE
                {
                mSTRIKE_INLINE(); 

                }
                break;
            case 9 :
                // OrgMode.g:1:105: ITALIC_INLINE
                {
                mITALIC_INLINE(); 

                }
                break;
            case 10 :
                // OrgMode.g:1:119: LINK_URL
                {
                mLINK_URL(); 

                }
                break;
            case 11 :
                // OrgMode.g:1:128: LINK_URL_DESC
                {
                mLINK_URL_DESC(); 

                }
                break;
            case 12 :
                // OrgMode.g:1:142: LE
                {
                mLE(); 

                }
                break;
            case 13 :
                // OrgMode.g:1:145: HEADER_STAR
                {
                mHEADER_STAR(); 

                }
                break;
            case 14 :
                // OrgMode.g:1:157: OL_START
                {
                mOL_START(); 

                }
                break;
            case 15 :
                // OrgMode.g:1:166: UL_START
                {
                mUL_START(); 

                }
                break;
            case 16 :
                // OrgMode.g:1:175: COL_START
                {
                mCOL_START(); 

                }
                break;
            case 17 :
                // OrgMode.g:1:185: SHARP_SETTING
                {
                mSHARP_SETTING(); 

                }
                break;
            case 18 :
                // OrgMode.g:1:199: WORD_NWS
                {
                mWORD_NWS(); 

                }
                break;
            case 19 :
                // OrgMode.g:1:208: NWS
                {
                mNWS(); 

                }
                break;

        }

    }


    protected DFA28 dfa28 = new DFA28(this);
    static final String DFA28_eotS =
        "\1\uffff\1\17\1\uffff\1\17\1\25\5\17\1\uffff\3\17\1\25\1\uffff\3"+
        "\17\1\uffff\1\25\1\uffff\2\25\3\17\1\uffff\3\17\1\uffff\1\25\4\17"+
        "\2\uffff\1\25\1\uffff\1\25\1\17\1\uffff\1\17\1\uffff\1\17\1\uffff"+
        "\1\17\1\uffff\2\17\2\uffff\4\17\2\25\4\17\1\106\1\17\1\uffff\1\17"+
        "\1\uffff\1\17\1\uffff\2\17\1\uffff\2\17\1\111\1\17\1\120\1\17\1"+
        "\uffff\1\122\1\uffff";
    static final String DFA28_eofS =
        "\123\uffff";
    static final String DFA28_minS =
        "\1\0\1\53\1\uffff\6\0\1\133\1\uffff\1\56\2\11\1\0\1\uffff\1\101"+
        "\1\0\1\11\1\uffff\1\0\1\uffff\5\0\1\uffff\2\0\1\11\1\uffff\1\0\3"+
        "\72\1\0\2\uffff\1\0\1\uffff\2\0\1\uffff\1\0\1\uffff\1\0\1\uffff"+
        "\1\0\1\uffff\1\0\1\133\1\0\1\uffff\1\72\1\11\1\72\11\0\1\133\1\72"+
        "\1\uffff\1\72\1\uffff\1\0\1\135\1\uffff\2\72\1\0\1\72\1\0\1\72\1"+
        "\uffff\1\0\1\uffff";
    static final String DFA28_maxS =
        "\1\uffff\1\53\1\uffff\6\uffff\1\133\1\uffff\1\71\2\40\1\uffff\1"+
        "\uffff\1\172\1\uffff\1\52\1\uffff\1\uffff\1\uffff\5\uffff\1\uffff"+
        "\2\uffff\1\40\1\uffff\1\uffff\3\172\1\uffff\2\uffff\1\uffff\1\uffff"+
        "\2\uffff\1\uffff\1\uffff\1\uffff\1\uffff\1\uffff\1\uffff\1\uffff"+
        "\1\uffff\1\135\1\uffff\1\uffff\1\172\1\40\1\172\11\uffff\1\135\1"+
        "\172\1\uffff\1\172\1\uffff\1\uffff\1\135\1\uffff\2\172\1\uffff\1"+
        "\172\1\uffff\1\172\1\uffff\1\uffff\1\uffff";
    static final String DFA28_acceptS =
        "\2\uffff\1\3\7\uffff\1\14\4\uffff\1\23\3\uffff\1\15\1\uffff\1\22"+
        "\5\uffff\1\17\3\uffff\1\20\5\uffff\1\4\1\15\1\uffff\1\5\2\uffff"+
        "\1\6\1\uffff\1\7\1\uffff\1\10\1\uffff\1\11\3\uffff\1\16\16\uffff"+
        "\1\21\1\uffff\1\12\2\uffff\1\13\6\uffff\1\2\1\uffff\1\1";
    static final String DFA28_specialS =
        "\1\3\2\uffff\1\22\1\41\1\14\1\30\1\42\1\5\5\uffff\1\44\2\uffff\1"+
        "\26\2\uffff\1\34\1\uffff\1\43\1\32\1\27\1\23\1\21\1\uffff\1\20\1"+
        "\33\2\uffff\1\0\3\uffff\1\35\2\uffff\1\40\1\uffff\1\45\1\1\1\uffff"+
        "\1\46\1\uffff\1\12\1\uffff\1\37\1\uffff\1\4\1\uffff\1\10\4\uffff"+
        "\1\6\1\2\1\13\1\25\1\47\1\31\1\16\1\36\1\11\5\uffff\1\7\4\uffff"+
        "\1\17\1\uffff\1\15\2\uffff\1\24\1\uffff}>";
    static final String[] DFA28_transitionS = {
            "\11\17\1\2\1\12\1\17\2\2\22\17\1\2\2\17\1\1\6\17\1\3\1\7\1\17"+
            "\1\14\1\17\1\10\12\13\1\15\2\17\1\5\3\17\32\16\1\11\3\17\1\4"+
            "\1\17\32\16\3\17\1\6\uff81\17",
            "\1\20",
            "",
            "\11\21\1\23\1\uffff\1\21\2\23\22\21\1\23\11\21\1\22\uffd5\21",
            "\11\27\2\uffff\1\27\2\uffff\22\27\1\uffff\40\27\32\24\4\27"+
            "\1\26\1\27\32\24\uff85\27",
            "\11\30\2\uffff\1\30\2\uffff\22\30\1\uffff\34\30\1\uffff\uffc2"+
            "\30",
            "\11\31\2\uffff\1\31\2\uffff\22\31\1\uffff\135\31\1\uffff\uff81"+
            "\31",
            "\11\32\1\33\1\uffff\1\32\2\33\22\32\1\33\12\32\1\uffff\uffd4"+
            "\32",
            "\11\34\2\uffff\1\34\2\uffff\22\34\1\uffff\16\34\1\uffff\uffd0"+
            "\34",
            "\1\35",
            "",
            "\1\36\1\uffff\12\13",
            "\1\33\2\uffff\2\33\22\uffff\1\33",
            "\1\37\2\uffff\2\37\22\uffff\1\37",
            "\11\40\2\uffff\1\40\2\uffff\22\40\1\uffff\40\40\32\26\4\40"+
            "\1\26\1\40\32\26\uff85\40",
            "",
            "\32\43\4\uffff\1\43\1\uffff\1\43\1\41\2\43\1\42\25\43",
            "\11\44\2\45\1\44\2\45\22\44\1\45\uffdf\44",
            "\1\46\2\uffff\2\46\22\uffff\1\46\11\uffff\1\22",
            "",
            "\11\51\2\50\1\51\2\50\22\51\1\50\40\51\32\47\4\51\1\47\1\51"+
            "\32\47\uff85\51",
            "",
            "\11\40\2\uffff\1\40\2\uffff\22\40\1\uffff\40\40\32\26\4\40"+
            "\1\26\1\40\32\26\uff85\40",
            "\11\51\2\50\1\51\2\50\22\51\1\50\uffdf\51",
            "\11\52\2\53\1\52\2\53\22\52\1\53\uffdf\52",
            "\11\54\2\55\1\54\2\55\22\54\1\55\uffdf\54",
            "\11\56\2\57\1\56\2\57\22\56\1\57\uffdf\56",
            "",
            "\11\60\2\61\1\60\2\61\22\60\1\61\uffdf\60",
            "\11\62\2\64\1\62\2\64\22\62\1\64\72\62\1\uffff\1\62\1\63\uffa2"+
            "\62",
            "\1\65\2\uffff\2\65\22\uffff\1\65",
            "",
            "\11\40\2\uffff\1\40\2\uffff\22\40\1\uffff\uffdf\40",
            "\1\67\6\uffff\32\43\4\uffff\1\43\1\uffff\4\43\1\66\25\43",
            "\1\67\6\uffff\32\43\4\uffff\1\43\1\uffff\15\43\1\70\14\43",
            "\1\67\6\uffff\32\43\4\uffff\1\43\1\uffff\32\43",
            "\11\44\2\45\1\44\2\45\22\44\1\45\11\44\1\71\uffd5\44",
            "",
            "",
            "\11\51\2\50\1\51\2\50\22\51\1\50\40\51\32\47\4\51\1\72\1\51"+
            "\32\47\uff85\51",
            "",
            "\11\51\2\50\1\51\2\50\22\51\1\50\76\51\1\73\uffa0\51",
            "\11\52\2\53\1\52\2\53\22\52\1\53\34\52\1\74\uffc2\52",
            "",
            "\11\54\2\55\1\54\2\55\22\54\1\55\135\54\1\75\uff81\54",
            "",
            "\11\56\2\57\1\56\2\57\22\56\1\57\12\56\1\76\uffd4\56",
            "",
            "\11\60\2\61\1\60\2\61\22\60\1\61\16\60\1\77\uffd0\60",
            "",
            "\11\62\2\64\1\62\2\64\22\62\1\64\72\62\1\uffff\1\62\1\63\uffa2"+
            "\62",
            "\1\101\1\uffff\1\100",
            "\133\64\1\uffff\1\64\1\102\uffa2\64",
            "",
            "\1\67\6\uffff\32\43\4\uffff\1\43\1\uffff\6\43\1\103\23\43",
            "\1\104\2\uffff\2\104\22\uffff\1\104",
            "\1\67\6\uffff\32\43\4\uffff\1\43\1\uffff\3\43\1\105\26\43",
            "\11\44\2\45\1\44\2\45\22\44\1\45\11\44\1\71\uffd5\44",
            "\11\51\2\50\1\51\2\50\22\51\1\50\40\51\32\47\4\51\1\72\1\51"+
            "\32\47\uff85\51",
            "\11\51\2\50\1\51\2\50\22\51\1\50\76\51\1\73\uffa0\51",
            "\11\52\2\53\1\52\2\53\22\52\1\53\34\52\1\74\uffc2\52",
            "\11\54\2\55\1\54\2\55\22\54\1\55\135\54\1\75\uff81\54",
            "\11\56\2\57\1\56\2\57\22\56\1\57\12\56\1\76\uffd4\56",
            "\11\60\2\61\1\60\2\61\22\60\1\61\16\60\1\77\uffd0\60",
            "\11\17\2\uffff\1\17\2\uffff\22\17\1\uffff\uffdf\17",
            "\11\107\2\111\1\107\2\111\22\107\1\111\72\107\1\uffff\1\107"+
            "\1\110\uffa2\107",
            "\1\111\1\uffff\1\106",
            "\1\67\6\uffff\32\43\4\uffff\1\43\1\uffff\10\43\1\112\21\43",
            "",
            "\1\67\6\uffff\32\43\4\uffff\1\113\1\uffff\32\43",
            "",
            "\11\107\2\111\1\107\2\111\22\107\1\111\72\107\1\uffff\1\107"+
            "\1\110\uffa2\107",
            "\1\114",
            "",
            "\1\67\6\uffff\32\43\4\uffff\1\43\1\uffff\15\43\1\115\14\43",
            "\1\67\6\uffff\32\116\4\uffff\1\116\1\uffff\32\116",
            "\11\17\2\uffff\1\17\2\uffff\22\17\1\uffff\uffdf\17",
            "\1\67\6\uffff\32\43\4\uffff\1\117\1\uffff\32\43",
            "\11\17\2\uffff\1\17\2\uffff\22\17\1\uffff\31\17\1\67\6\17\32"+
            "\116\4\17\1\116\1\17\32\116\uff85\17",
            "\1\67\6\uffff\32\121\4\uffff\1\121\1\uffff\32\121",
            "",
            "\11\17\2\uffff\1\17\2\uffff\22\17\1\uffff\31\17\1\67\6\17\32"+
            "\121\4\17\1\121\1\17\32\121\uff85\17",
            ""
    };

    static final short[] DFA28_eot = DFA.unpackEncodedString(DFA28_eotS);
    static final short[] DFA28_eof = DFA.unpackEncodedString(DFA28_eofS);
    static final char[] DFA28_min = DFA.unpackEncodedStringToUnsignedChars(DFA28_minS);
    static final char[] DFA28_max = DFA.unpackEncodedStringToUnsignedChars(DFA28_maxS);
    static final short[] DFA28_accept = DFA.unpackEncodedString(DFA28_acceptS);
    static final short[] DFA28_special = DFA.unpackEncodedString(DFA28_specialS);
    static final short[][] DFA28_transition;

    static {
        int numStates = DFA28_transitionS.length;
        DFA28_transition = new short[numStates][];
        for (int i=0; i<numStates; i++) {
            DFA28_transition[i] = DFA.unpackEncodedString(DFA28_transitionS[i]);
        }
    }

    class DFA28 extends DFA {

        public DFA28(BaseRecognizer recognizer) {
            this.recognizer = recognizer;
            this.decisionNumber = 28;
            this.eot = DFA28_eot;
            this.eof = DFA28_eof;
            this.min = DFA28_min;
            this.max = DFA28_max;
            this.accept = DFA28_accept;
            this.special = DFA28_special;
            this.transition = DFA28_transition;
        }
        public String getDescription() {
            return "1:1: Tokens : ( BEG_BLOCK | END_BLOCK | WS | BOLD_INLINE | UNDERLINED_INLINE | CODE_INLINE | VERBATIM_INLINE | STRIKE_INLINE | ITALIC_INLINE | LINK_URL | LINK_URL_DESC | LE | HEADER_STAR | OL_START | UL_START | COL_START | SHARP_SETTING | WORD_NWS | NWS );";
        }
        public int specialStateTransition(int s, IntStream _input) throws NoViableAltException {
            IntStream input = _input;
        	int _s = s;
            switch ( s ) {
                    case 0 : 
                        int LA28_32 = input.LA(1);

                        s = -1;
                        if ( ((LA28_32>='\u0000' && LA28_32<='\b')||LA28_32=='\u000B'||(LA28_32>='\u000E' && LA28_32<='\u001F')||(LA28_32>='!' && LA28_32<='\uFFFF')) ) {s = 32;}

                        else s = 21;

                        if ( s>=0 ) return s;
                        break;
                    case 1 : 
                        int LA28_42 = input.LA(1);

                        s = -1;
                        if ( (LA28_42=='=') ) {s = 60;}

                        else if ( ((LA28_42>='\u0000' && LA28_42<='\b')||LA28_42=='\u000B'||(LA28_42>='\u000E' && LA28_42<='\u001F')||(LA28_42>='!' && LA28_42<='<')||(LA28_42>='>' && LA28_42<='\uFFFF')) ) {s = 42;}

                        else if ( ((LA28_42>='\t' && LA28_42<='\n')||(LA28_42>='\f' && LA28_42<='\r')||LA28_42==' ') ) {s = 43;}

                        else s = 15;

                        if ( s>=0 ) return s;
                        break;
                    case 2 : 
                        int LA28_58 = input.LA(1);

                        s = -1;
                        if ( (LA28_58=='_') ) {s = 58;}

                        else if ( ((LA28_58>='A' && LA28_58<='Z')||(LA28_58>='a' && LA28_58<='z')) ) {s = 39;}

                        else if ( ((LA28_58>='\t' && LA28_58<='\n')||(LA28_58>='\f' && LA28_58<='\r')||LA28_58==' ') ) {s = 40;}

                        else if ( ((LA28_58>='\u0000' && LA28_58<='\b')||LA28_58=='\u000B'||(LA28_58>='\u000E' && LA28_58<='\u001F')||(LA28_58>='!' && LA28_58<='@')||(LA28_58>='[' && LA28_58<='^')||LA28_58=='`'||(LA28_58>='{' && LA28_58<='\uFFFF')) ) {s = 41;}

                        else s = 21;

                        if ( s>=0 ) return s;
                        break;
                    case 3 : 
                        int LA28_0 = input.LA(1);

                        s = -1;
                        if ( (LA28_0=='#') ) {s = 1;}

                        else if ( (LA28_0=='\t'||(LA28_0>='\f' && LA28_0<='\r')||LA28_0==' ') ) {s = 2;}

                        else if ( (LA28_0=='*') ) {s = 3;}

                        else if ( (LA28_0=='_') ) {s = 4;}

                        else if ( (LA28_0=='=') ) {s = 5;}

                        else if ( (LA28_0=='~') ) {s = 6;}

                        else if ( (LA28_0=='+') ) {s = 7;}

                        else if ( (LA28_0=='/') ) {s = 8;}

                        else if ( (LA28_0=='[') ) {s = 9;}

                        else if ( (LA28_0=='\n') ) {s = 10;}

                        else if ( ((LA28_0>='0' && LA28_0<='9')) ) {s = 11;}

                        else if ( (LA28_0=='-') ) {s = 12;}

                        else if ( (LA28_0==':') ) {s = 13;}

                        else if ( ((LA28_0>='A' && LA28_0<='Z')||(LA28_0>='a' && LA28_0<='z')) ) {s = 14;}

                        else if ( ((LA28_0>='\u0000' && LA28_0<='\b')||LA28_0=='\u000B'||(LA28_0>='\u000E' && LA28_0<='\u001F')||(LA28_0>='!' && LA28_0<='\"')||(LA28_0>='$' && LA28_0<=')')||LA28_0==','||LA28_0=='.'||(LA28_0>=';' && LA28_0<='<')||(LA28_0>='>' && LA28_0<='@')||(LA28_0>='\\' && LA28_0<='^')||LA28_0=='`'||(LA28_0>='{' && LA28_0<='}')||(LA28_0>='\u007F' && LA28_0<='\uFFFF')) ) {s = 15;}

                        if ( s>=0 ) return s;
                        break;
                    case 4 : 
                        int LA28_50 = input.LA(1);

                        s = -1;
                        if ( (LA28_50==']') ) {s = 51;}

                        else if ( ((LA28_50>='\u0000' && LA28_50<='\b')||LA28_50=='\u000B'||(LA28_50>='\u000E' && LA28_50<='\u001F')||(LA28_50>='!' && LA28_50<='Z')||LA28_50=='\\'||(LA28_50>='^' && LA28_50<='\uFFFF')) ) {s = 50;}

                        else if ( ((LA28_50>='\t' && LA28_50<='\n')||(LA28_50>='\f' && LA28_50<='\r')||LA28_50==' ') ) {s = 52;}

                        else s = 15;

                        if ( s>=0 ) return s;
                        break;
                    case 5 : 
                        int LA28_8 = input.LA(1);

                        s = -1;
                        if ( ((LA28_8>='\u0000' && LA28_8<='\b')||LA28_8=='\u000B'||(LA28_8>='\u000E' && LA28_8<='\u001F')||(LA28_8>='!' && LA28_8<='.')||(LA28_8>='0' && LA28_8<='\uFFFF')) ) {s = 28;}

                        else s = 15;

                        if ( s>=0 ) return s;
                        break;
                    case 6 : 
                        int LA28_57 = input.LA(1);

                        s = -1;
                        if ( (LA28_57=='*') ) {s = 57;}

                        else if ( ((LA28_57>='\u0000' && LA28_57<='\b')||LA28_57=='\u000B'||(LA28_57>='\u000E' && LA28_57<='\u001F')||(LA28_57>='!' && LA28_57<=')')||(LA28_57>='+' && LA28_57<='\uFFFF')) ) {s = 36;}

                        else if ( ((LA28_57>='\t' && LA28_57<='\n')||(LA28_57>='\f' && LA28_57<='\r')||LA28_57==' ') ) {s = 37;}

                        else s = 15;

                        if ( s>=0 ) return s;
                        break;
                    case 7 : 
                        int LA28_71 = input.LA(1);

                        s = -1;
                        if ( (LA28_71==']') ) {s = 72;}

                        else if ( ((LA28_71>='\u0000' && LA28_71<='\b')||LA28_71=='\u000B'||(LA28_71>='\u000E' && LA28_71<='\u001F')||(LA28_71>='!' && LA28_71<='Z')||LA28_71=='\\'||(LA28_71>='^' && LA28_71<='\uFFFF')) ) {s = 71;}

                        else if ( ((LA28_71>='\t' && LA28_71<='\n')||(LA28_71>='\f' && LA28_71<='\r')||LA28_71==' ') ) {s = 73;}

                        else s = 15;

                        if ( s>=0 ) return s;
                        break;
                    case 8 : 
                        int LA28_52 = input.LA(1);

                        s = -1;
                        if ( (LA28_52==']') ) {s = 66;}

                        else if ( ((LA28_52>='\u0000' && LA28_52<='Z')||LA28_52=='\\'||(LA28_52>='^' && LA28_52<='\uFFFF')) ) {s = 52;}

                        if ( s>=0 ) return s;
                        break;
                    case 9 : 
                        int LA28_65 = input.LA(1);

                        s = -1;
                        if ( ((LA28_65>='\u0000' && LA28_65<='\b')||LA28_65=='\u000B'||(LA28_65>='\u000E' && LA28_65<='\u001F')||(LA28_65>='!' && LA28_65<='Z')||LA28_65=='\\'||(LA28_65>='^' && LA28_65<='\uFFFF')) ) {s = 71;}

                        else if ( (LA28_65==']') ) {s = 72;}

                        else if ( ((LA28_65>='\t' && LA28_65<='\n')||(LA28_65>='\f' && LA28_65<='\r')||LA28_65==' ') ) {s = 73;}

                        else s = 15;

                        if ( s>=0 ) return s;
                        break;
                    case 10 : 
                        int LA28_46 = input.LA(1);

                        s = -1;
                        if ( (LA28_46=='+') ) {s = 62;}

                        else if ( ((LA28_46>='\u0000' && LA28_46<='\b')||LA28_46=='\u000B'||(LA28_46>='\u000E' && LA28_46<='\u001F')||(LA28_46>='!' && LA28_46<='*')||(LA28_46>=',' && LA28_46<='\uFFFF')) ) {s = 46;}

                        else if ( ((LA28_46>='\t' && LA28_46<='\n')||(LA28_46>='\f' && LA28_46<='\r')||LA28_46==' ') ) {s = 47;}

                        else s = 15;

                        if ( s>=0 ) return s;
                        break;
                    case 11 : 
                        int LA28_59 = input.LA(1);

                        s = -1;
                        if ( (LA28_59=='_') ) {s = 59;}

                        else if ( ((LA28_59>='\u0000' && LA28_59<='\b')||LA28_59=='\u000B'||(LA28_59>='\u000E' && LA28_59<='\u001F')||(LA28_59>='!' && LA28_59<='^')||(LA28_59>='`' && LA28_59<='\uFFFF')) ) {s = 41;}

                        else if ( ((LA28_59>='\t' && LA28_59<='\n')||(LA28_59>='\f' && LA28_59<='\r')||LA28_59==' ') ) {s = 40;}

                        else s = 21;

                        if ( s>=0 ) return s;
                        break;
                    case 12 : 
                        int LA28_5 = input.LA(1);

                        s = -1;
                        if ( ((LA28_5>='\u0000' && LA28_5<='\b')||LA28_5=='\u000B'||(LA28_5>='\u000E' && LA28_5<='\u001F')||(LA28_5>='!' && LA28_5<='<')||(LA28_5>='>' && LA28_5<='\uFFFF')) ) {s = 24;}

                        else s = 15;

                        if ( s>=0 ) return s;
                        break;
                    case 13 : 
                        int LA28_78 = input.LA(1);

                        s = -1;
                        if ( ((LA28_78>='A' && LA28_78<='Z')||LA28_78=='_'||(LA28_78>='a' && LA28_78<='z')) ) {s = 78;}

                        else if ( (LA28_78==':') ) {s = 55;}

                        else if ( ((LA28_78>='\u0000' && LA28_78<='\b')||LA28_78=='\u000B'||(LA28_78>='\u000E' && LA28_78<='\u001F')||(LA28_78>='!' && LA28_78<='9')||(LA28_78>=';' && LA28_78<='@')||(LA28_78>='[' && LA28_78<='^')||LA28_78=='`'||(LA28_78>='{' && LA28_78<='\uFFFF')) ) {s = 15;}

                        else s = 80;

                        if ( s>=0 ) return s;
                        break;
                    case 14 : 
                        int LA28_63 = input.LA(1);

                        s = -1;
                        if ( (LA28_63=='/') ) {s = 63;}

                        else if ( ((LA28_63>='\u0000' && LA28_63<='\b')||LA28_63=='\u000B'||(LA28_63>='\u000E' && LA28_63<='\u001F')||(LA28_63>='!' && LA28_63<='.')||(LA28_63>='0' && LA28_63<='\uFFFF')) ) {s = 48;}

                        else if ( ((LA28_63>='\t' && LA28_63<='\n')||(LA28_63>='\f' && LA28_63<='\r')||LA28_63==' ') ) {s = 49;}

                        else s = 15;

                        if ( s>=0 ) return s;
                        break;
                    case 15 : 
                        int LA28_76 = input.LA(1);

                        s = -1;
                        if ( ((LA28_76>='\u0000' && LA28_76<='\b')||LA28_76=='\u000B'||(LA28_76>='\u000E' && LA28_76<='\u001F')||(LA28_76>='!' && LA28_76<='\uFFFF')) ) {s = 15;}

                        else s = 73;

                        if ( s>=0 ) return s;
                        break;
                    case 16 : 
                        int LA28_28 = input.LA(1);

                        s = -1;
                        if ( ((LA28_28>='\u0000' && LA28_28<='\b')||LA28_28=='\u000B'||(LA28_28>='\u000E' && LA28_28<='\u001F')||(LA28_28>='!' && LA28_28<='\uFFFF')) ) {s = 48;}

                        else if ( ((LA28_28>='\t' && LA28_28<='\n')||(LA28_28>='\f' && LA28_28<='\r')||LA28_28==' ') ) {s = 49;}

                        else s = 15;

                        if ( s>=0 ) return s;
                        break;
                    case 17 : 
                        int LA28_26 = input.LA(1);

                        s = -1;
                        if ( ((LA28_26>='\u0000' && LA28_26<='\b')||LA28_26=='\u000B'||(LA28_26>='\u000E' && LA28_26<='\u001F')||(LA28_26>='!' && LA28_26<='\uFFFF')) ) {s = 46;}

                        else if ( ((LA28_26>='\t' && LA28_26<='\n')||(LA28_26>='\f' && LA28_26<='\r')||LA28_26==' ') ) {s = 47;}

                        else s = 15;

                        if ( s>=0 ) return s;
                        break;
                    case 18 : 
                        int LA28_3 = input.LA(1);

                        s = -1;
                        if ( ((LA28_3>='\u0000' && LA28_3<='\b')||LA28_3=='\u000B'||(LA28_3>='\u000E' && LA28_3<='\u001F')||(LA28_3>='!' && LA28_3<=')')||(LA28_3>='+' && LA28_3<='\uFFFF')) ) {s = 17;}

                        else if ( (LA28_3=='*') ) {s = 18;}

                        else if ( (LA28_3=='\t'||(LA28_3>='\f' && LA28_3<='\r')||LA28_3==' ') ) {s = 19;}

                        else s = 15;

                        if ( s>=0 ) return s;
                        break;
                    case 19 : 
                        int LA28_25 = input.LA(1);

                        s = -1;
                        if ( ((LA28_25>='\u0000' && LA28_25<='\b')||LA28_25=='\u000B'||(LA28_25>='\u000E' && LA28_25<='\u001F')||(LA28_25>='!' && LA28_25<='\uFFFF')) ) {s = 44;}

                        else if ( ((LA28_25>='\t' && LA28_25<='\n')||(LA28_25>='\f' && LA28_25<='\r')||LA28_25==' ') ) {s = 45;}

                        else s = 15;

                        if ( s>=0 ) return s;
                        break;
                    case 20 : 
                        int LA28_81 = input.LA(1);

                        s = -1;
                        if ( ((LA28_81>='A' && LA28_81<='Z')||LA28_81=='_'||(LA28_81>='a' && LA28_81<='z')) ) {s = 81;}

                        else if ( (LA28_81==':') ) {s = 55;}

                        else if ( ((LA28_81>='\u0000' && LA28_81<='\b')||LA28_81=='\u000B'||(LA28_81>='\u000E' && LA28_81<='\u001F')||(LA28_81>='!' && LA28_81<='9')||(LA28_81>=';' && LA28_81<='@')||(LA28_81>='[' && LA28_81<='^')||LA28_81=='`'||(LA28_81>='{' && LA28_81<='\uFFFF')) ) {s = 15;}

                        else s = 82;

                        if ( s>=0 ) return s;
                        break;
                    case 21 : 
                        int LA28_60 = input.LA(1);

                        s = -1;
                        if ( (LA28_60=='=') ) {s = 60;}

                        else if ( ((LA28_60>='\t' && LA28_60<='\n')||(LA28_60>='\f' && LA28_60<='\r')||LA28_60==' ') ) {s = 43;}

                        else if ( ((LA28_60>='\u0000' && LA28_60<='\b')||LA28_60=='\u000B'||(LA28_60>='\u000E' && LA28_60<='\u001F')||(LA28_60>='!' && LA28_60<='<')||(LA28_60>='>' && LA28_60<='\uFFFF')) ) {s = 42;}

                        else s = 15;

                        if ( s>=0 ) return s;
                        break;
                    case 22 : 
                        int LA28_17 = input.LA(1);

                        s = -1;
                        if ( ((LA28_17>='\u0000' && LA28_17<='\b')||LA28_17=='\u000B'||(LA28_17>='\u000E' && LA28_17<='\u001F')||(LA28_17>='!' && LA28_17<='\uFFFF')) ) {s = 36;}

                        else if ( ((LA28_17>='\t' && LA28_17<='\n')||(LA28_17>='\f' && LA28_17<='\r')||LA28_17==' ') ) {s = 37;}

                        else s = 15;

                        if ( s>=0 ) return s;
                        break;
                    case 23 : 
                        int LA28_24 = input.LA(1);

                        s = -1;
                        if ( ((LA28_24>='\u0000' && LA28_24<='\b')||LA28_24=='\u000B'||(LA28_24>='\u000E' && LA28_24<='\u001F')||(LA28_24>='!' && LA28_24<='\uFFFF')) ) {s = 42;}

                        else if ( ((LA28_24>='\t' && LA28_24<='\n')||(LA28_24>='\f' && LA28_24<='\r')||LA28_24==' ') ) {s = 43;}

                        else s = 15;

                        if ( s>=0 ) return s;
                        break;
                    case 24 : 
                        int LA28_6 = input.LA(1);

                        s = -1;
                        if ( ((LA28_6>='\u0000' && LA28_6<='\b')||LA28_6=='\u000B'||(LA28_6>='\u000E' && LA28_6<='\u001F')||(LA28_6>='!' && LA28_6<='}')||(LA28_6>='\u007F' && LA28_6<='\uFFFF')) ) {s = 25;}

                        else s = 15;

                        if ( s>=0 ) return s;
                        break;
                    case 25 : 
                        int LA28_62 = input.LA(1);

                        s = -1;
                        if ( (LA28_62=='+') ) {s = 62;}

                        else if ( ((LA28_62>='\t' && LA28_62<='\n')||(LA28_62>='\f' && LA28_62<='\r')||LA28_62==' ') ) {s = 47;}

                        else if ( ((LA28_62>='\u0000' && LA28_62<='\b')||LA28_62=='\u000B'||(LA28_62>='\u000E' && LA28_62<='\u001F')||(LA28_62>='!' && LA28_62<='*')||(LA28_62>=',' && LA28_62<='\uFFFF')) ) {s = 46;}

                        else s = 15;

                        if ( s>=0 ) return s;
                        break;
                    case 26 : 
                        int LA28_23 = input.LA(1);

                        s = -1;
                        if ( ((LA28_23>='\u0000' && LA28_23<='\b')||LA28_23=='\u000B'||(LA28_23>='\u000E' && LA28_23<='\u001F')||(LA28_23>='!' && LA28_23<='\uFFFF')) ) {s = 41;}

                        else if ( ((LA28_23>='\t' && LA28_23<='\n')||(LA28_23>='\f' && LA28_23<='\r')||LA28_23==' ') ) {s = 40;}

                        else s = 21;

                        if ( s>=0 ) return s;
                        break;
                    case 27 : 
                        int LA28_29 = input.LA(1);

                        s = -1;
                        if ( ((LA28_29>='\u0000' && LA28_29<='\b')||LA28_29=='\u000B'||(LA28_29>='\u000E' && LA28_29<='\u001F')||(LA28_29>='!' && LA28_29<='Z')||LA28_29=='\\'||(LA28_29>='^' && LA28_29<='\uFFFF')) ) {s = 50;}

                        else if ( (LA28_29==']') ) {s = 51;}

                        else if ( ((LA28_29>='\t' && LA28_29<='\n')||(LA28_29>='\f' && LA28_29<='\r')||LA28_29==' ') ) {s = 52;}

                        else s = 15;

                        if ( s>=0 ) return s;
                        break;
                    case 28 : 
                        int LA28_20 = input.LA(1);

                        s = -1;
                        if ( ((LA28_20>='A' && LA28_20<='Z')||LA28_20=='_'||(LA28_20>='a' && LA28_20<='z')) ) {s = 39;}

                        else if ( ((LA28_20>='\t' && LA28_20<='\n')||(LA28_20>='\f' && LA28_20<='\r')||LA28_20==' ') ) {s = 40;}

                        else if ( ((LA28_20>='\u0000' && LA28_20<='\b')||LA28_20=='\u000B'||(LA28_20>='\u000E' && LA28_20<='\u001F')||(LA28_20>='!' && LA28_20<='@')||(LA28_20>='[' && LA28_20<='^')||LA28_20=='`'||(LA28_20>='{' && LA28_20<='\uFFFF')) ) {s = 41;}

                        else s = 21;

                        if ( s>=0 ) return s;
                        break;
                    case 29 : 
                        int LA28_36 = input.LA(1);

                        s = -1;
                        if ( (LA28_36=='*') ) {s = 57;}

                        else if ( ((LA28_36>='\u0000' && LA28_36<='\b')||LA28_36=='\u000B'||(LA28_36>='\u000E' && LA28_36<='\u001F')||(LA28_36>='!' && LA28_36<=')')||(LA28_36>='+' && LA28_36<='\uFFFF')) ) {s = 36;}

                        else if ( ((LA28_36>='\t' && LA28_36<='\n')||(LA28_36>='\f' && LA28_36<='\r')||LA28_36==' ') ) {s = 37;}

                        else s = 15;

                        if ( s>=0 ) return s;
                        break;
                    case 30 : 
                        int LA28_64 = input.LA(1);

                        s = -1;
                        if ( ((LA28_64>='\u0000' && LA28_64<='\b')||LA28_64=='\u000B'||(LA28_64>='\u000E' && LA28_64<='\u001F')||(LA28_64>='!' && LA28_64<='\uFFFF')) ) {s = 15;}

                        else s = 70;

                        if ( s>=0 ) return s;
                        break;
                    case 31 : 
                        int LA28_48 = input.LA(1);

                        s = -1;
                        if ( (LA28_48=='/') ) {s = 63;}

                        else if ( ((LA28_48>='\u0000' && LA28_48<='\b')||LA28_48=='\u000B'||(LA28_48>='\u000E' && LA28_48<='\u001F')||(LA28_48>='!' && LA28_48<='.')||(LA28_48>='0' && LA28_48<='\uFFFF')) ) {s = 48;}

                        else if ( ((LA28_48>='\t' && LA28_48<='\n')||(LA28_48>='\f' && LA28_48<='\r')||LA28_48==' ') ) {s = 49;}

                        else s = 15;

                        if ( s>=0 ) return s;
                        break;
                    case 32 : 
                        int LA28_39 = input.LA(1);

                        s = -1;
                        if ( (LA28_39=='_') ) {s = 58;}

                        else if ( ((LA28_39>='A' && LA28_39<='Z')||(LA28_39>='a' && LA28_39<='z')) ) {s = 39;}

                        else if ( ((LA28_39>='\t' && LA28_39<='\n')||(LA28_39>='\f' && LA28_39<='\r')||LA28_39==' ') ) {s = 40;}

                        else if ( ((LA28_39>='\u0000' && LA28_39<='\b')||LA28_39=='\u000B'||(LA28_39>='\u000E' && LA28_39<='\u001F')||(LA28_39>='!' && LA28_39<='@')||(LA28_39>='[' && LA28_39<='^')||LA28_39=='`'||(LA28_39>='{' && LA28_39<='\uFFFF')) ) {s = 41;}

                        else s = 21;

                        if ( s>=0 ) return s;
                        break;
                    case 33 : 
                        int LA28_4 = input.LA(1);

                        s = -1;
                        if ( ((LA28_4>='A' && LA28_4<='Z')||(LA28_4>='a' && LA28_4<='z')) ) {s = 20;}

                        else if ( (LA28_4=='_') ) {s = 22;}

                        else if ( ((LA28_4>='\u0000' && LA28_4<='\b')||LA28_4=='\u000B'||(LA28_4>='\u000E' && LA28_4<='\u001F')||(LA28_4>='!' && LA28_4<='@')||(LA28_4>='[' && LA28_4<='^')||LA28_4=='`'||(LA28_4>='{' && LA28_4<='\uFFFF')) ) {s = 23;}

                        else s = 21;

                        if ( s>=0 ) return s;
                        break;
                    case 34 : 
                        int LA28_7 = input.LA(1);

                        s = -1;
                        if ( ((LA28_7>='\u0000' && LA28_7<='\b')||LA28_7=='\u000B'||(LA28_7>='\u000E' && LA28_7<='\u001F')||(LA28_7>='!' && LA28_7<='*')||(LA28_7>=',' && LA28_7<='\uFFFF')) ) {s = 26;}

                        else if ( (LA28_7=='\t'||(LA28_7>='\f' && LA28_7<='\r')||LA28_7==' ') ) {s = 27;}

                        else s = 15;

                        if ( s>=0 ) return s;
                        break;
                    case 35 : 
                        int LA28_22 = input.LA(1);

                        s = -1;
                        if ( ((LA28_22>='A' && LA28_22<='Z')||LA28_22=='_'||(LA28_22>='a' && LA28_22<='z')) ) {s = 22;}

                        else if ( ((LA28_22>='\u0000' && LA28_22<='\b')||LA28_22=='\u000B'||(LA28_22>='\u000E' && LA28_22<='\u001F')||(LA28_22>='!' && LA28_22<='@')||(LA28_22>='[' && LA28_22<='^')||LA28_22=='`'||(LA28_22>='{' && LA28_22<='\uFFFF')) ) {s = 32;}

                        else s = 21;

                        if ( s>=0 ) return s;
                        break;
                    case 36 : 
                        int LA28_14 = input.LA(1);

                        s = -1;
                        if ( ((LA28_14>='A' && LA28_14<='Z')||LA28_14=='_'||(LA28_14>='a' && LA28_14<='z')) ) {s = 22;}

                        else if ( ((LA28_14>='\u0000' && LA28_14<='\b')||LA28_14=='\u000B'||(LA28_14>='\u000E' && LA28_14<='\u001F')||(LA28_14>='!' && LA28_14<='@')||(LA28_14>='[' && LA28_14<='^')||LA28_14=='`'||(LA28_14>='{' && LA28_14<='\uFFFF')) ) {s = 32;}

                        else s = 21;

                        if ( s>=0 ) return s;
                        break;
                    case 37 : 
                        int LA28_41 = input.LA(1);

                        s = -1;
                        if ( (LA28_41=='_') ) {s = 59;}

                        else if ( ((LA28_41>='\u0000' && LA28_41<='\b')||LA28_41=='\u000B'||(LA28_41>='\u000E' && LA28_41<='\u001F')||(LA28_41>='!' && LA28_41<='^')||(LA28_41>='`' && LA28_41<='\uFFFF')) ) {s = 41;}

                        else if ( ((LA28_41>='\t' && LA28_41<='\n')||(LA28_41>='\f' && LA28_41<='\r')||LA28_41==' ') ) {s = 40;}

                        else s = 21;

                        if ( s>=0 ) return s;
                        break;
                    case 38 : 
                        int LA28_44 = input.LA(1);

                        s = -1;
                        if ( (LA28_44=='~') ) {s = 61;}

                        else if ( ((LA28_44>='\u0000' && LA28_44<='\b')||LA28_44=='\u000B'||(LA28_44>='\u000E' && LA28_44<='\u001F')||(LA28_44>='!' && LA28_44<='}')||(LA28_44>='\u007F' && LA28_44<='\uFFFF')) ) {s = 44;}

                        else if ( ((LA28_44>='\t' && LA28_44<='\n')||(LA28_44>='\f' && LA28_44<='\r')||LA28_44==' ') ) {s = 45;}

                        else s = 15;

                        if ( s>=0 ) return s;
                        break;
                    case 39 : 
                        int LA28_61 = input.LA(1);

                        s = -1;
                        if ( (LA28_61=='~') ) {s = 61;}

                        else if ( ((LA28_61>='\t' && LA28_61<='\n')||(LA28_61>='\f' && LA28_61<='\r')||LA28_61==' ') ) {s = 45;}

                        else if ( ((LA28_61>='\u0000' && LA28_61<='\b')||LA28_61=='\u000B'||(LA28_61>='\u000E' && LA28_61<='\u001F')||(LA28_61>='!' && LA28_61<='}')||(LA28_61>='\u007F' && LA28_61<='\uFFFF')) ) {s = 44;}

                        else s = 15;

                        if ( s>=0 ) return s;
                        break;
            }
            NoViableAltException nvae =
                new NoViableAltException(getDescription(), 28, _s, input);
            error(nvae);
            throw nvae;
        }
    }
 

}