// $ANTLR 3.2 debian-7 Test1.g 2012-12-20 20:15:47

import org.antlr.runtime.*;
import java.util.Stack;
import java.util.List;
import java.util.ArrayList;

public class Test1Lexer extends Lexer {
    public static final int WORD=4;
    public static final int ID=5;
    public static final int EOF=-1;

    // delegates
    // delegators

    public Test1Lexer() {;} 
    public Test1Lexer(CharStream input) {
        this(input, new RecognizerSharedState());
    }
    public Test1Lexer(CharStream input, RecognizerSharedState state) {
        super(input,state);

    }
    public String getGrammarFileName() { return "Test1.g"; }

    // $ANTLR start "ID"
    public final void mID() throws RecognitionException {
        try {
            int _type = ID;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // Test1.g:5:4: ( 'x' )
            // Test1.g:5:6: 'x'
            {
            match('x'); 

            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "ID"

    // $ANTLR start "WORD"
    public final void mWORD() throws RecognitionException {
        try {
            int _type = WORD;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // Test1.g:6:6: ( 'y' )
            // Test1.g:6:8: 'y'
            {
            match('y'); 

            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "WORD"

    public void mTokens() throws RecognitionException {
        // Test1.g:1:8: ( ID | WORD )
        int alt1=2;
        int LA1_0 = input.LA(1);

        if ( (LA1_0=='x') ) {
            alt1=1;
        }
        else if ( (LA1_0=='y') ) {
            alt1=2;
        }
        else {
            NoViableAltException nvae =
                new NoViableAltException("", 1, 0, input);

            throw nvae;
        }
        switch (alt1) {
            case 1 :
                // Test1.g:1:10: ID
                {
                mID(); 

                }
                break;
            case 2 :
                // Test1.g:1:13: WORD
                {
                mWORD(); 

                }
                break;

        }

    }


 

}