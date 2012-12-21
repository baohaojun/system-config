// $ANTLR 3.2 debian-7 OrgMode.g 2012-12-20 20:58:49

import org.antlr.runtime.*;
import java.util.Stack;
import java.util.List;
import java.util.ArrayList;

public class OrgModeLexer extends Lexer {
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
    public static final int ITALIC_INLINE=21;
    public static final int LINK_URL_DESC=23;
    public static final int NL=12;
    public static final int EQ=11;
    public static final int BEGIN_SRC=6;

      protected boolean bol = true;
      protected boolean assertIsKeyword = true;

      boolean looking_backat_white_space() {
          return true;
      }

      boolean looking_at_white_space() {
          return true;
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

    // $ANTLR start "STAR"
    public final void mSTAR() throws RecognitionException {
        try {
            int _type = STAR;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // OrgMode.g:16:6: ( '*' )
            // OrgMode.g:16:8: '*'
            {
            match('*'); 

            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "STAR"

    // $ANTLR start "PLUS"
    public final void mPLUS() throws RecognitionException {
        try {
            int _type = PLUS;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // OrgMode.g:17:6: ( '+' )
            // OrgMode.g:17:8: '+'
            {
            match('+'); 

            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "PLUS"

    // $ANTLR start "MINUS"
    public final void mMINUS() throws RecognitionException {
        try {
            int _type = MINUS;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // OrgMode.g:18:7: ( '-' )
            // OrgMode.g:18:9: '-'
            {
            match('-'); 

            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "MINUS"

    // $ANTLR start "EQ"
    public final void mEQ() throws RecognitionException {
        try {
            int _type = EQ;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // OrgMode.g:19:4: ( '=' )
            // OrgMode.g:19:6: '='
            {
            match('='); 

            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "EQ"

    // $ANTLR start "WORD"
    public final void mWORD() throws RecognitionException {
        try {
            int _type = WORD;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // OrgMode.g:88:6: ( ( 'a' .. 'z' | 'A' .. 'Z' )+ )
            // OrgMode.g:88:8: ( 'a' .. 'z' | 'A' .. 'Z' )+
            {
            // OrgMode.g:88:8: ( 'a' .. 'z' | 'A' .. 'Z' )+
            int cnt1=0;
            loop1:
            do {
                int alt1=2;
                int LA1_0 = input.LA(1);

                if ( ((LA1_0>='A' && LA1_0<='Z')||(LA1_0>='a' && LA1_0<='z')) ) {
                    alt1=1;
                }


                switch (alt1) {
            	case 1 :
            	    // OrgMode.g:
            	    {
            	    if ( (input.LA(1)>='A' && input.LA(1)<='Z')||(input.LA(1)>='a' && input.LA(1)<='z') ) {
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

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "WORD"

    // $ANTLR start "BEGIN_EXAMPLE"
    public final void mBEGIN_EXAMPLE() throws RecognitionException {
        try {
            int _type = BEGIN_EXAMPLE;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // OrgMode.g:90:15: ( '#+begin_example' )
            // OrgMode.g:90:17: '#+begin_example'
            {
            match("#+begin_example"); 


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "BEGIN_EXAMPLE"

    // $ANTLR start "END_EXAMPLE"
    public final void mEND_EXAMPLE() throws RecognitionException {
        try {
            int _type = END_EXAMPLE;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // OrgMode.g:92:13: ( '#+end_example' )
            // OrgMode.g:92:15: '#+end_example'
            {
            match("#+end_example"); 


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "END_EXAMPLE"

    // $ANTLR start "BEGIN_SRC"
    public final void mBEGIN_SRC() throws RecognitionException {
        try {
            int _type = BEGIN_SRC;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // OrgMode.g:94:11: ( '#+begin_src' )
            // OrgMode.g:94:13: '#+begin_src'
            {
            match("#+begin_src"); 


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "BEGIN_SRC"

    // $ANTLR start "END_SRC"
    public final void mEND_SRC() throws RecognitionException {
        try {
            int _type = END_SRC;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // OrgMode.g:95:9: ( '#+end_src' )
            // OrgMode.g:95:11: '#+end_src'
            {
            match("#+end_src"); 


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "END_SRC"

    // $ANTLR start "DOUBLE_NL"
    public final void mDOUBLE_NL() throws RecognitionException {
        try {
            int _type = DOUBLE_NL;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // OrgMode.g:97:11: ( '\\n' ( ( ' ' | '\\f' | '\\r' | '\\t' )* '\\n' )+ )
            // OrgMode.g:97:13: '\\n' ( ( ' ' | '\\f' | '\\r' | '\\t' )* '\\n' )+
            {
            match('\n'); 
            // OrgMode.g:97:18: ( ( ' ' | '\\f' | '\\r' | '\\t' )* '\\n' )+
            int cnt3=0;
            loop3:
            do {
                int alt3=2;
                int LA3_0 = input.LA(1);

                if ( ((LA3_0>='\t' && LA3_0<='\n')||(LA3_0>='\f' && LA3_0<='\r')||LA3_0==' ') ) {
                    alt3=1;
                }


                switch (alt3) {
            	case 1 :
            	    // OrgMode.g:97:20: ( ' ' | '\\f' | '\\r' | '\\t' )* '\\n'
            	    {
            	    // OrgMode.g:97:20: ( ' ' | '\\f' | '\\r' | '\\t' )*
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
            	    	    break loop2;
            	        }
            	    } while (true);

            	    match('\n'); 

            	    }
            	    break;

            	default :
            	    if ( cnt3 >= 1 ) break loop3;
                        EarlyExitException eee =
                            new EarlyExitException(3, input);
                        throw eee;
                }
                cnt3++;
            } while (true);


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "DOUBLE_NL"

    // $ANTLR start "NL"
    public final void mNL() throws RecognitionException {
        try {
            int _type = NL;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // OrgMode.g:98:4: ( '\\n' )
            // OrgMode.g:98:6: '\\n'
            {
            match('\n'); 

            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "NL"

    // $ANTLR start "WS"
    public final void mWS() throws RecognitionException {
        try {
            int _type = WS;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // OrgMode.g:100:4: ( ( ' ' | '\\t' | '\\r' )+ )
            // OrgMode.g:100:8: ( ' ' | '\\t' | '\\r' )+
            {
            // OrgMode.g:100:8: ( ' ' | '\\t' | '\\r' )+
            int cnt4=0;
            loop4:
            do {
                int alt4=2;
                int LA4_0 = input.LA(1);

                if ( (LA4_0=='\t'||LA4_0=='\r'||LA4_0==' ') ) {
                    alt4=1;
                }


                switch (alt4) {
            	case 1 :
            	    // OrgMode.g:
            	    {
            	    if ( input.LA(1)=='\t'||input.LA(1)=='\r'||input.LA(1)==' ' ) {
            	        input.consume();

            	    }
            	    else {
            	        MismatchedSetException mse = new MismatchedSetException(null,input);
            	        recover(mse);
            	        throw mse;}


            	    }
            	    break;

            	default :
            	    if ( cnt4 >= 1 ) break loop4;
                        EarlyExitException eee =
                            new EarlyExitException(4, input);
                        throw eee;
                }
                cnt4++;
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

    // $ANTLR start "BOLD_INLINE"
    public final void mBOLD_INLINE() throws RecognitionException {
        try {
            int _type = BOLD_INLINE;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // OrgMode.g:114:13: ({...}? '*' ~ ( '*' | ' ' | '\\t' | '\\r' | '\\n' ) ( . )* ~ ( ' ' | '\\t' | '\\r' | '\\n' ) ( '*' )+ {...}?)
            // OrgMode.g:114:15: {...}? '*' ~ ( '*' | ' ' | '\\t' | '\\r' | '\\n' ) ( . )* ~ ( ' ' | '\\t' | '\\r' | '\\n' ) ( '*' )+ {...}?
            {
            if ( !((looking_backat_white_space())) ) {
                throw new FailedPredicateException(input, "BOLD_INLINE", "looking_backat_white_space()");
            }
            match('*'); 
            if ( (input.LA(1)>='\u0000' && input.LA(1)<='\b')||(input.LA(1)>='\u000B' && input.LA(1)<='\f')||(input.LA(1)>='\u000E' && input.LA(1)<='\u001F')||(input.LA(1)>='!' && input.LA(1)<=')')||(input.LA(1)>='+' && input.LA(1)<='\uFFFF') ) {
                input.consume();

            }
            else {
                MismatchedSetException mse = new MismatchedSetException(null,input);
                recover(mse);
                throw mse;}

            // OrgMode.g:114:85: ( . )*
            loop5:
            do {
                int alt5=2;
                int LA5_0 = input.LA(1);

                if ( ((LA5_0>='\u0000' && LA5_0<='\b')||(LA5_0>='\u000B' && LA5_0<='\f')||(LA5_0>='\u000E' && LA5_0<='\u001F')||(LA5_0>='!' && LA5_0<='\uFFFF')) ) {
                    int LA5_1 = input.LA(2);

                    if ( (LA5_1=='*') ) {
                        alt5=2;
                    }
                    else if ( ((LA5_1>='\u0000' && LA5_1<=')')||(LA5_1>='+' && LA5_1<='\uFFFF')) ) {
                        alt5=1;
                    }


                }
                else if ( ((LA5_0>='\t' && LA5_0<='\n')||LA5_0=='\r'||LA5_0==' ') ) {
                    alt5=1;
                }


                switch (alt5) {
            	case 1 :
            	    // OrgMode.g:114:85: .
            	    {
            	    matchAny(); 

            	    }
            	    break;

            	default :
            	    break loop5;
                }
            } while (true);

            if ( (input.LA(1)>='\u0000' && input.LA(1)<='\b')||(input.LA(1)>='\u000B' && input.LA(1)<='\f')||(input.LA(1)>='\u000E' && input.LA(1)<='\u001F')||(input.LA(1)>='!' && input.LA(1)<='\uFFFF') ) {
                input.consume();

            }
            else {
                MismatchedSetException mse = new MismatchedSetException(null,input);
                recover(mse);
                throw mse;}

            // OrgMode.g:114:116: ( '*' )+
            int cnt6=0;
            loop6:
            do {
                int alt6=2;
                int LA6_0 = input.LA(1);

                if ( (LA6_0=='*') ) {
                    alt6=1;
                }


                switch (alt6) {
            	case 1 :
            	    // OrgMode.g:114:116: '*'
            	    {
            	    match('*'); 

            	    }
            	    break;

            	default :
            	    if ( cnt6 >= 1 ) break loop6;
                        EarlyExitException eee =
                            new EarlyExitException(6, input);
                        throw eee;
                }
                cnt6++;
            } while (true);

            if ( !((looking_at_white_space())) ) {
                throw new FailedPredicateException(input, "BOLD_INLINE", "looking_at_white_space()");
            }
            System.out.println("matched inline: " + getText());

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
            // OrgMode.g:115:19: ({...}? '_' ~ ( '_' | ' ' | '\\t' | '\\r' | '\\n' ) ( . )* ~ ( ' ' | '\\t' | '\\r' | '\\n' ) ( '_' )+ {...}?)
            // OrgMode.g:115:21: {...}? '_' ~ ( '_' | ' ' | '\\t' | '\\r' | '\\n' ) ( . )* ~ ( ' ' | '\\t' | '\\r' | '\\n' ) ( '_' )+ {...}?
            {
            if ( !((looking_backat_white_space())) ) {
                throw new FailedPredicateException(input, "UNDERLINED_INLINE", "looking_backat_white_space()");
            }
            match('_'); 
            if ( (input.LA(1)>='\u0000' && input.LA(1)<='\b')||(input.LA(1)>='\u000B' && input.LA(1)<='\f')||(input.LA(1)>='\u000E' && input.LA(1)<='\u001F')||(input.LA(1)>='!' && input.LA(1)<='^')||(input.LA(1)>='`' && input.LA(1)<='\uFFFF') ) {
                input.consume();

            }
            else {
                MismatchedSetException mse = new MismatchedSetException(null,input);
                recover(mse);
                throw mse;}

            // OrgMode.g:115:91: ( . )*
            loop7:
            do {
                int alt7=2;
                int LA7_0 = input.LA(1);

                if ( ((LA7_0>='\u0000' && LA7_0<='\b')||(LA7_0>='\u000B' && LA7_0<='\f')||(LA7_0>='\u000E' && LA7_0<='\u001F')||(LA7_0>='!' && LA7_0<='\uFFFF')) ) {
                    int LA7_1 = input.LA(2);

                    if ( (LA7_1=='_') ) {
                        alt7=2;
                    }
                    else if ( ((LA7_1>='\u0000' && LA7_1<='^')||(LA7_1>='`' && LA7_1<='\uFFFF')) ) {
                        alt7=1;
                    }


                }
                else if ( ((LA7_0>='\t' && LA7_0<='\n')||LA7_0=='\r'||LA7_0==' ') ) {
                    alt7=1;
                }


                switch (alt7) {
            	case 1 :
            	    // OrgMode.g:115:91: .
            	    {
            	    matchAny(); 

            	    }
            	    break;

            	default :
            	    break loop7;
                }
            } while (true);

            if ( (input.LA(1)>='\u0000' && input.LA(1)<='\b')||(input.LA(1)>='\u000B' && input.LA(1)<='\f')||(input.LA(1)>='\u000E' && input.LA(1)<='\u001F')||(input.LA(1)>='!' && input.LA(1)<='\uFFFF') ) {
                input.consume();

            }
            else {
                MismatchedSetException mse = new MismatchedSetException(null,input);
                recover(mse);
                throw mse;}

            // OrgMode.g:115:122: ( '_' )+
            int cnt8=0;
            loop8:
            do {
                int alt8=2;
                int LA8_0 = input.LA(1);

                if ( (LA8_0=='_') ) {
                    alt8=1;
                }


                switch (alt8) {
            	case 1 :
            	    // OrgMode.g:115:122: '_'
            	    {
            	    match('_'); 

            	    }
            	    break;

            	default :
            	    if ( cnt8 >= 1 ) break loop8;
                        EarlyExitException eee =
                            new EarlyExitException(8, input);
                        throw eee;
                }
                cnt8++;
            } while (true);

            if ( !((looking_at_white_space())) ) {
                throw new FailedPredicateException(input, "UNDERLINED_INLINE", "looking_at_white_space()");
            }
            System.out.println("matched inline: " + getText());

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
            // OrgMode.g:116:13: ({...}? '=' ~ ( '=' | ' ' | '\\t' | '\\r' | '\\n' ) ( . )* ~ ( ' ' | '\\t' | '\\r' | '\\n' ) ( '=' )+ {...}?)
            // OrgMode.g:116:15: {...}? '=' ~ ( '=' | ' ' | '\\t' | '\\r' | '\\n' ) ( . )* ~ ( ' ' | '\\t' | '\\r' | '\\n' ) ( '=' )+ {...}?
            {
            if ( !((looking_backat_white_space())) ) {
                throw new FailedPredicateException(input, "CODE_INLINE", "looking_backat_white_space()");
            }
            match('='); 
            if ( (input.LA(1)>='\u0000' && input.LA(1)<='\b')||(input.LA(1)>='\u000B' && input.LA(1)<='\f')||(input.LA(1)>='\u000E' && input.LA(1)<='\u001F')||(input.LA(1)>='!' && input.LA(1)<='<')||(input.LA(1)>='>' && input.LA(1)<='\uFFFF') ) {
                input.consume();

            }
            else {
                MismatchedSetException mse = new MismatchedSetException(null,input);
                recover(mse);
                throw mse;}

            // OrgMode.g:116:85: ( . )*
            loop9:
            do {
                int alt9=2;
                int LA9_0 = input.LA(1);

                if ( ((LA9_0>='\u0000' && LA9_0<='\b')||(LA9_0>='\u000B' && LA9_0<='\f')||(LA9_0>='\u000E' && LA9_0<='\u001F')||(LA9_0>='!' && LA9_0<='\uFFFF')) ) {
                    int LA9_1 = input.LA(2);

                    if ( (LA9_1=='=') ) {
                        alt9=2;
                    }
                    else if ( ((LA9_1>='\u0000' && LA9_1<='<')||(LA9_1>='>' && LA9_1<='\uFFFF')) ) {
                        alt9=1;
                    }


                }
                else if ( ((LA9_0>='\t' && LA9_0<='\n')||LA9_0=='\r'||LA9_0==' ') ) {
                    alt9=1;
                }


                switch (alt9) {
            	case 1 :
            	    // OrgMode.g:116:85: .
            	    {
            	    matchAny(); 

            	    }
            	    break;

            	default :
            	    break loop9;
                }
            } while (true);

            if ( (input.LA(1)>='\u0000' && input.LA(1)<='\b')||(input.LA(1)>='\u000B' && input.LA(1)<='\f')||(input.LA(1)>='\u000E' && input.LA(1)<='\u001F')||(input.LA(1)>='!' && input.LA(1)<='\uFFFF') ) {
                input.consume();

            }
            else {
                MismatchedSetException mse = new MismatchedSetException(null,input);
                recover(mse);
                throw mse;}

            // OrgMode.g:116:116: ( '=' )+
            int cnt10=0;
            loop10:
            do {
                int alt10=2;
                int LA10_0 = input.LA(1);

                if ( (LA10_0=='=') ) {
                    alt10=1;
                }


                switch (alt10) {
            	case 1 :
            	    // OrgMode.g:116:116: '='
            	    {
            	    match('='); 

            	    }
            	    break;

            	default :
            	    if ( cnt10 >= 1 ) break loop10;
                        EarlyExitException eee =
                            new EarlyExitException(10, input);
                        throw eee;
                }
                cnt10++;
            } while (true);

            if ( !((looking_at_white_space())) ) {
                throw new FailedPredicateException(input, "CODE_INLINE", "looking_at_white_space()");
            }
            System.out.println("matched inline: " + getText());

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
            // OrgMode.g:117:17: ({...}? '~' ~ ( '~' | ' ' | '\\t' | '\\r' | '\\n' ) ( . )* ~ ( ' ' | '\\t' | '\\r' | '\\n' ) ( '~' )+ {...}?)
            // OrgMode.g:117:19: {...}? '~' ~ ( '~' | ' ' | '\\t' | '\\r' | '\\n' ) ( . )* ~ ( ' ' | '\\t' | '\\r' | '\\n' ) ( '~' )+ {...}?
            {
            if ( !((looking_backat_white_space())) ) {
                throw new FailedPredicateException(input, "VERBATIM_INLINE", "looking_backat_white_space()");
            }
            match('~'); 
            if ( (input.LA(1)>='\u0000' && input.LA(1)<='\b')||(input.LA(1)>='\u000B' && input.LA(1)<='\f')||(input.LA(1)>='\u000E' && input.LA(1)<='\u001F')||(input.LA(1)>='!' && input.LA(1)<='}')||(input.LA(1)>='\u007F' && input.LA(1)<='\uFFFF') ) {
                input.consume();

            }
            else {
                MismatchedSetException mse = new MismatchedSetException(null,input);
                recover(mse);
                throw mse;}

            // OrgMode.g:117:89: ( . )*
            loop11:
            do {
                int alt11=2;
                int LA11_0 = input.LA(1);

                if ( ((LA11_0>='\u0000' && LA11_0<='\b')||(LA11_0>='\u000B' && LA11_0<='\f')||(LA11_0>='\u000E' && LA11_0<='\u001F')||(LA11_0>='!' && LA11_0<='\uFFFF')) ) {
                    int LA11_1 = input.LA(2);

                    if ( (LA11_1=='~') ) {
                        alt11=2;
                    }
                    else if ( ((LA11_1>='\u0000' && LA11_1<='}')||(LA11_1>='\u007F' && LA11_1<='\uFFFF')) ) {
                        alt11=1;
                    }


                }
                else if ( ((LA11_0>='\t' && LA11_0<='\n')||LA11_0=='\r'||LA11_0==' ') ) {
                    alt11=1;
                }


                switch (alt11) {
            	case 1 :
            	    // OrgMode.g:117:89: .
            	    {
            	    matchAny(); 

            	    }
            	    break;

            	default :
            	    break loop11;
                }
            } while (true);

            if ( (input.LA(1)>='\u0000' && input.LA(1)<='\b')||(input.LA(1)>='\u000B' && input.LA(1)<='\f')||(input.LA(1)>='\u000E' && input.LA(1)<='\u001F')||(input.LA(1)>='!' && input.LA(1)<='\uFFFF') ) {
                input.consume();

            }
            else {
                MismatchedSetException mse = new MismatchedSetException(null,input);
                recover(mse);
                throw mse;}

            // OrgMode.g:117:120: ( '~' )+
            int cnt12=0;
            loop12:
            do {
                int alt12=2;
                int LA12_0 = input.LA(1);

                if ( (LA12_0=='~') ) {
                    alt12=1;
                }


                switch (alt12) {
            	case 1 :
            	    // OrgMode.g:117:120: '~'
            	    {
            	    match('~'); 

            	    }
            	    break;

            	default :
            	    if ( cnt12 >= 1 ) break loop12;
                        EarlyExitException eee =
                            new EarlyExitException(12, input);
                        throw eee;
                }
                cnt12++;
            } while (true);

            if ( !((looking_at_white_space())) ) {
                throw new FailedPredicateException(input, "VERBATIM_INLINE", "looking_at_white_space()");
            }
            System.out.println("matched inline: " + getText());

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
            // OrgMode.g:118:15: ({...}? '+' ~ ( '+' | ' ' | '\\t' | '\\r' | '\\n' ) ( . )* ~ ( ' ' | '\\t' | '\\r' | '\\n' ) ( '+' )+ {...}?)
            // OrgMode.g:118:17: {...}? '+' ~ ( '+' | ' ' | '\\t' | '\\r' | '\\n' ) ( . )* ~ ( ' ' | '\\t' | '\\r' | '\\n' ) ( '+' )+ {...}?
            {
            if ( !((looking_backat_white_space())) ) {
                throw new FailedPredicateException(input, "STRIKE_INLINE", "looking_backat_white_space()");
            }
            match('+'); 
            if ( (input.LA(1)>='\u0000' && input.LA(1)<='\b')||(input.LA(1)>='\u000B' && input.LA(1)<='\f')||(input.LA(1)>='\u000E' && input.LA(1)<='\u001F')||(input.LA(1)>='!' && input.LA(1)<='*')||(input.LA(1)>=',' && input.LA(1)<='\uFFFF') ) {
                input.consume();

            }
            else {
                MismatchedSetException mse = new MismatchedSetException(null,input);
                recover(mse);
                throw mse;}

            // OrgMode.g:118:87: ( . )*
            loop13:
            do {
                int alt13=2;
                int LA13_0 = input.LA(1);

                if ( ((LA13_0>='\u0000' && LA13_0<='\b')||(LA13_0>='\u000B' && LA13_0<='\f')||(LA13_0>='\u000E' && LA13_0<='\u001F')||(LA13_0>='!' && LA13_0<='\uFFFF')) ) {
                    int LA13_1 = input.LA(2);

                    if ( (LA13_1=='+') ) {
                        alt13=2;
                    }
                    else if ( ((LA13_1>='\u0000' && LA13_1<='*')||(LA13_1>=',' && LA13_1<='\uFFFF')) ) {
                        alt13=1;
                    }


                }
                else if ( ((LA13_0>='\t' && LA13_0<='\n')||LA13_0=='\r'||LA13_0==' ') ) {
                    alt13=1;
                }


                switch (alt13) {
            	case 1 :
            	    // OrgMode.g:118:87: .
            	    {
            	    matchAny(); 

            	    }
            	    break;

            	default :
            	    break loop13;
                }
            } while (true);

            if ( (input.LA(1)>='\u0000' && input.LA(1)<='\b')||(input.LA(1)>='\u000B' && input.LA(1)<='\f')||(input.LA(1)>='\u000E' && input.LA(1)<='\u001F')||(input.LA(1)>='!' && input.LA(1)<='\uFFFF') ) {
                input.consume();

            }
            else {
                MismatchedSetException mse = new MismatchedSetException(null,input);
                recover(mse);
                throw mse;}

            // OrgMode.g:118:118: ( '+' )+
            int cnt14=0;
            loop14:
            do {
                int alt14=2;
                int LA14_0 = input.LA(1);

                if ( (LA14_0=='+') ) {
                    alt14=1;
                }


                switch (alt14) {
            	case 1 :
            	    // OrgMode.g:118:118: '+'
            	    {
            	    match('+'); 

            	    }
            	    break;

            	default :
            	    if ( cnt14 >= 1 ) break loop14;
                        EarlyExitException eee =
                            new EarlyExitException(14, input);
                        throw eee;
                }
                cnt14++;
            } while (true);

            if ( !((looking_at_white_space())) ) {
                throw new FailedPredicateException(input, "STRIKE_INLINE", "looking_at_white_space()");
            }
            System.out.println("matched inline: " + getText());

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
            // OrgMode.g:119:15: ({...}? '/' ~ ( '/' | ' ' | '\\t' | '\\r' | '\\n' ) ( . )* ~ ( ' ' | '\\t' | '\\r' | '\\n' ) ( '/' )+ {...}?)
            // OrgMode.g:119:17: {...}? '/' ~ ( '/' | ' ' | '\\t' | '\\r' | '\\n' ) ( . )* ~ ( ' ' | '\\t' | '\\r' | '\\n' ) ( '/' )+ {...}?
            {
            if ( !((looking_backat_white_space())) ) {
                throw new FailedPredicateException(input, "ITALIC_INLINE", "looking_backat_white_space()");
            }
            match('/'); 
            if ( (input.LA(1)>='\u0000' && input.LA(1)<='\b')||(input.LA(1)>='\u000B' && input.LA(1)<='\f')||(input.LA(1)>='\u000E' && input.LA(1)<='\u001F')||(input.LA(1)>='!' && input.LA(1)<='.')||(input.LA(1)>='0' && input.LA(1)<='\uFFFF') ) {
                input.consume();

            }
            else {
                MismatchedSetException mse = new MismatchedSetException(null,input);
                recover(mse);
                throw mse;}

            // OrgMode.g:119:87: ( . )*
            loop15:
            do {
                int alt15=2;
                int LA15_0 = input.LA(1);

                if ( ((LA15_0>='\u0000' && LA15_0<='\b')||(LA15_0>='\u000B' && LA15_0<='\f')||(LA15_0>='\u000E' && LA15_0<='\u001F')||(LA15_0>='!' && LA15_0<='\uFFFF')) ) {
                    int LA15_1 = input.LA(2);

                    if ( (LA15_1=='/') ) {
                        alt15=2;
                    }
                    else if ( ((LA15_1>='\u0000' && LA15_1<='.')||(LA15_1>='0' && LA15_1<='\uFFFF')) ) {
                        alt15=1;
                    }


                }
                else if ( ((LA15_0>='\t' && LA15_0<='\n')||LA15_0=='\r'||LA15_0==' ') ) {
                    alt15=1;
                }


                switch (alt15) {
            	case 1 :
            	    // OrgMode.g:119:87: .
            	    {
            	    matchAny(); 

            	    }
            	    break;

            	default :
            	    break loop15;
                }
            } while (true);

            if ( (input.LA(1)>='\u0000' && input.LA(1)<='\b')||(input.LA(1)>='\u000B' && input.LA(1)<='\f')||(input.LA(1)>='\u000E' && input.LA(1)<='\u001F')||(input.LA(1)>='!' && input.LA(1)<='\uFFFF') ) {
                input.consume();

            }
            else {
                MismatchedSetException mse = new MismatchedSetException(null,input);
                recover(mse);
                throw mse;}

            // OrgMode.g:119:118: ( '/' )+
            int cnt16=0;
            loop16:
            do {
                int alt16=2;
                int LA16_0 = input.LA(1);

                if ( (LA16_0=='/') ) {
                    alt16=1;
                }


                switch (alt16) {
            	case 1 :
            	    // OrgMode.g:119:118: '/'
            	    {
            	    match('/'); 

            	    }
            	    break;

            	default :
            	    if ( cnt16 >= 1 ) break loop16;
                        EarlyExitException eee =
                            new EarlyExitException(16, input);
                        throw eee;
                }
                cnt16++;
            } while (true);

            if ( !((looking_at_white_space())) ) {
                throw new FailedPredicateException(input, "ITALIC_INLINE", "looking_at_white_space()");
            }
            System.out.println("matched inline: " + getText());

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
            // OrgMode.g:121:10: ( '[[' (~ ( '[' | ']' ) )* ']]' )
            // OrgMode.g:121:12: '[[' (~ ( '[' | ']' ) )* ']]'
            {
            match("[["); 

            // OrgMode.g:121:17: (~ ( '[' | ']' ) )*
            loop17:
            do {
                int alt17=2;
                int LA17_0 = input.LA(1);

                if ( ((LA17_0>='\u0000' && LA17_0<='Z')||LA17_0=='\\'||(LA17_0>='^' && LA17_0<='\uFFFF')) ) {
                    alt17=1;
                }


                switch (alt17) {
            	case 1 :
            	    // OrgMode.g:121:17: ~ ( '[' | ']' )
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
            	    break loop17;
                }
            } while (true);

            match("]]"); 

            System.out.println("matched link url: " + getText());

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
            // OrgMode.g:122:15: ( '[[' (~ ( '[' | ']' ) )* '][' (~ ( '[' | ']' ) )* ']]' )
            // OrgMode.g:122:17: '[[' (~ ( '[' | ']' ) )* '][' (~ ( '[' | ']' ) )* ']]'
            {
            match("[["); 

            // OrgMode.g:122:22: (~ ( '[' | ']' ) )*
            loop18:
            do {
                int alt18=2;
                int LA18_0 = input.LA(1);

                if ( ((LA18_0>='\u0000' && LA18_0<='Z')||LA18_0=='\\'||(LA18_0>='^' && LA18_0<='\uFFFF')) ) {
                    alt18=1;
                }


                switch (alt18) {
            	case 1 :
            	    // OrgMode.g:122:22: ~ ( '[' | ']' )
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
            	    break loop18;
                }
            } while (true);

            match("]["); 

            // OrgMode.g:122:39: (~ ( '[' | ']' ) )*
            loop19:
            do {
                int alt19=2;
                int LA19_0 = input.LA(1);

                if ( ((LA19_0>='\u0000' && LA19_0<='Z')||LA19_0=='\\'||(LA19_0>='^' && LA19_0<='\uFFFF')) ) {
                    alt19=1;
                }


                switch (alt19) {
            	case 1 :
            	    // OrgMode.g:122:39: ~ ( '[' | ']' )
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
            	    break loop19;
                }
            } while (true);

            match("]]"); 

            System.out.println("matched link desc: " + getText());

            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "LINK_URL_DESC"

    public void mTokens() throws RecognitionException {
        // OrgMode.g:1:8: ( STAR | PLUS | MINUS | EQ | WORD | BEGIN_EXAMPLE | END_EXAMPLE | BEGIN_SRC | END_SRC | DOUBLE_NL | NL | WS | BOLD_INLINE | UNDERLINED_INLINE | CODE_INLINE | VERBATIM_INLINE | STRIKE_INLINE | ITALIC_INLINE | LINK_URL | LINK_URL_DESC )
        int alt20=20;
        alt20 = dfa20.predict(input);
        switch (alt20) {
            case 1 :
                // OrgMode.g:1:10: STAR
                {
                mSTAR(); 

                }
                break;
            case 2 :
                // OrgMode.g:1:15: PLUS
                {
                mPLUS(); 

                }
                break;
            case 3 :
                // OrgMode.g:1:20: MINUS
                {
                mMINUS(); 

                }
                break;
            case 4 :
                // OrgMode.g:1:26: EQ
                {
                mEQ(); 

                }
                break;
            case 5 :
                // OrgMode.g:1:29: WORD
                {
                mWORD(); 

                }
                break;
            case 6 :
                // OrgMode.g:1:34: BEGIN_EXAMPLE
                {
                mBEGIN_EXAMPLE(); 

                }
                break;
            case 7 :
                // OrgMode.g:1:48: END_EXAMPLE
                {
                mEND_EXAMPLE(); 

                }
                break;
            case 8 :
                // OrgMode.g:1:60: BEGIN_SRC
                {
                mBEGIN_SRC(); 

                }
                break;
            case 9 :
                // OrgMode.g:1:70: END_SRC
                {
                mEND_SRC(); 

                }
                break;
            case 10 :
                // OrgMode.g:1:78: DOUBLE_NL
                {
                mDOUBLE_NL(); 

                }
                break;
            case 11 :
                // OrgMode.g:1:88: NL
                {
                mNL(); 

                }
                break;
            case 12 :
                // OrgMode.g:1:91: WS
                {
                mWS(); 

                }
                break;
            case 13 :
                // OrgMode.g:1:94: BOLD_INLINE
                {
                mBOLD_INLINE(); 

                }
                break;
            case 14 :
                // OrgMode.g:1:106: UNDERLINED_INLINE
                {
                mUNDERLINED_INLINE(); 

                }
                break;
            case 15 :
                // OrgMode.g:1:124: CODE_INLINE
                {
                mCODE_INLINE(); 

                }
                break;
            case 16 :
                // OrgMode.g:1:136: VERBATIM_INLINE
                {
                mVERBATIM_INLINE(); 

                }
                break;
            case 17 :
                // OrgMode.g:1:152: STRIKE_INLINE
                {
                mSTRIKE_INLINE(); 

                }
                break;
            case 18 :
                // OrgMode.g:1:166: ITALIC_INLINE
                {
                mITALIC_INLINE(); 

                }
                break;
            case 19 :
                // OrgMode.g:1:180: LINK_URL
                {
                mLINK_URL(); 

                }
                break;
            case 20 :
                // OrgMode.g:1:189: LINK_URL_DESC
                {
                mLINK_URL_DESC(); 

                }
                break;

        }

    }


    protected DFA20 dfa20 = new DFA20(this);
    static final String DFA20_eotS =
        "\1\uffff\1\16\1\20\1\uffff\1\22\2\uffff\1\24\41\uffff";
    static final String DFA20_eofS =
        "\51\uffff";
    static final String DFA20_minS =
        "\1\11\2\0\1\uffff\1\0\1\uffff\1\53\1\11\4\uffff\1\133\6\uffff\1"+
        "\142\2\uffff\1\0\1\145\1\156\1\0\1\133\1\147\1\144\2\uffff\1\151"+
        "\1\137\1\156\1\145\1\137\2\uffff\1\145\2\uffff";
    static final String DFA20_maxS =
        "\1\176\2\uffff\1\uffff\1\uffff\1\uffff\1\53\1\40\4\uffff\1\133\6"+
        "\uffff\1\145\2\uffff\1\uffff\1\145\1\156\1\uffff\1\135\1\147\1\144"+
        "\2\uffff\1\151\1\137\1\156\1\163\1\137\2\uffff\1\163\2\uffff";
    static final String DFA20_acceptS =
        "\3\uffff\1\3\1\uffff\1\5\2\uffff\1\14\1\16\1\20\1\22\1\uffff\1\15"+
        "\1\1\1\21\1\2\1\17\1\4\1\uffff\1\13\1\12\7\uffff\1\23\1\24\5\uffff"+
        "\1\7\1\11\1\uffff\1\6\1\10";
    static final String DFA20_specialS =
        "\1\uffff\1\1\1\2\1\uffff\1\0\21\uffff\1\4\2\uffff\1\3\17\uffff}>";
    static final String[] DFA20_transitionS = {
            "\1\10\1\7\2\uffff\1\10\22\uffff\1\10\2\uffff\1\6\6\uffff\1\1"+
            "\1\2\1\uffff\1\3\1\uffff\1\13\15\uffff\1\4\3\uffff\32\5\1\14"+
            "\3\uffff\1\11\1\uffff\32\5\3\uffff\1\12",
            "\11\15\2\uffff\2\15\1\uffff\22\15\1\uffff\11\15\1\uffff\uffd5"+
            "\15",
            "\11\17\2\uffff\2\17\1\uffff\22\17\1\uffff\12\17\1\uffff\uffd4"+
            "\17",
            "",
            "\11\21\2\uffff\2\21\1\uffff\22\21\1\uffff\34\21\1\uffff\uffc2"+
            "\21",
            "",
            "\1\23",
            "\2\25\1\uffff\2\25\22\uffff\1\25",
            "",
            "",
            "",
            "",
            "\1\26",
            "",
            "",
            "",
            "",
            "",
            "",
            "\1\27\2\uffff\1\30",
            "",
            "",
            "\133\31\1\uffff\1\31\1\32\uffa2\31",
            "\1\33",
            "\1\34",
            "\133\31\1\uffff\1\31\1\32\uffa2\31",
            "\1\36\1\uffff\1\35",
            "\1\37",
            "\1\40",
            "",
            "",
            "\1\41",
            "\1\42",
            "\1\43",
            "\1\44\15\uffff\1\45",
            "\1\46",
            "",
            "",
            "\1\47\15\uffff\1\50",
            "",
            ""
    };

    static final short[] DFA20_eot = DFA.unpackEncodedString(DFA20_eotS);
    static final short[] DFA20_eof = DFA.unpackEncodedString(DFA20_eofS);
    static final char[] DFA20_min = DFA.unpackEncodedStringToUnsignedChars(DFA20_minS);
    static final char[] DFA20_max = DFA.unpackEncodedStringToUnsignedChars(DFA20_maxS);
    static final short[] DFA20_accept = DFA.unpackEncodedString(DFA20_acceptS);
    static final short[] DFA20_special = DFA.unpackEncodedString(DFA20_specialS);
    static final short[][] DFA20_transition;

    static {
        int numStates = DFA20_transitionS.length;
        DFA20_transition = new short[numStates][];
        for (int i=0; i<numStates; i++) {
            DFA20_transition[i] = DFA.unpackEncodedString(DFA20_transitionS[i]);
        }
    }

    class DFA20 extends DFA {

        public DFA20(BaseRecognizer recognizer) {
            this.recognizer = recognizer;
            this.decisionNumber = 20;
            this.eot = DFA20_eot;
            this.eof = DFA20_eof;
            this.min = DFA20_min;
            this.max = DFA20_max;
            this.accept = DFA20_accept;
            this.special = DFA20_special;
            this.transition = DFA20_transition;
        }
        public String getDescription() {
            return "1:1: Tokens : ( STAR | PLUS | MINUS | EQ | WORD | BEGIN_EXAMPLE | END_EXAMPLE | BEGIN_SRC | END_SRC | DOUBLE_NL | NL | WS | BOLD_INLINE | UNDERLINED_INLINE | CODE_INLINE | VERBATIM_INLINE | STRIKE_INLINE | ITALIC_INLINE | LINK_URL | LINK_URL_DESC );";
        }
        public int specialStateTransition(int s, IntStream _input) throws NoViableAltException {
            IntStream input = _input;
        	int _s = s;
            switch ( s ) {
                    case 0 : 
                        int LA20_4 = input.LA(1);

                        s = -1;
                        if ( ((LA20_4>='\u0000' && LA20_4<='\b')||(LA20_4>='\u000B' && LA20_4<='\f')||(LA20_4>='\u000E' && LA20_4<='\u001F')||(LA20_4>='!' && LA20_4<='<')||(LA20_4>='>' && LA20_4<='\uFFFF')) ) {s = 17;}

                        else s = 18;

                        if ( s>=0 ) return s;
                        break;
                    case 1 : 
                        int LA20_1 = input.LA(1);

                        s = -1;
                        if ( ((LA20_1>='\u0000' && LA20_1<='\b')||(LA20_1>='\u000B' && LA20_1<='\f')||(LA20_1>='\u000E' && LA20_1<='\u001F')||(LA20_1>='!' && LA20_1<=')')||(LA20_1>='+' && LA20_1<='\uFFFF')) ) {s = 13;}

                        else s = 14;

                        if ( s>=0 ) return s;
                        break;
                    case 2 : 
                        int LA20_2 = input.LA(1);

                        s = -1;
                        if ( ((LA20_2>='\u0000' && LA20_2<='\b')||(LA20_2>='\u000B' && LA20_2<='\f')||(LA20_2>='\u000E' && LA20_2<='\u001F')||(LA20_2>='!' && LA20_2<='*')||(LA20_2>=',' && LA20_2<='\uFFFF')) ) {s = 15;}

                        else s = 16;

                        if ( s>=0 ) return s;
                        break;
                    case 3 : 
                        int LA20_25 = input.LA(1);

                        s = -1;
                        if ( (LA20_25==']') ) {s = 26;}

                        else if ( ((LA20_25>='\u0000' && LA20_25<='Z')||LA20_25=='\\'||(LA20_25>='^' && LA20_25<='\uFFFF')) ) {s = 25;}

                        if ( s>=0 ) return s;
                        break;
                    case 4 : 
                        int LA20_22 = input.LA(1);

                        s = -1;
                        if ( ((LA20_22>='\u0000' && LA20_22<='Z')||LA20_22=='\\'||(LA20_22>='^' && LA20_22<='\uFFFF')) ) {s = 25;}

                        else if ( (LA20_22==']') ) {s = 26;}

                        if ( s>=0 ) return s;
                        break;
            }
            NoViableAltException nvae =
                new NoViableAltException(getDescription(), 20, _s, input);
            error(nvae);
            throw nvae;
        }
    }
 

}