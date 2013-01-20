// $ANTLR 3.2 debian-7 Test1.g 2012-12-20 20:15:47

import org.antlr.runtime.*;
import java.util.Stack;
import java.util.List;
import java.util.ArrayList;

public class Test1Parser extends Parser {
    public static final String[] tokenNames = new String[] {
        "<invalid>", "<EOR>", "<DOWN>", "<UP>", "WORD", "ID"
    };
    public static final int WORD=4;
    public static final int ID=5;
    public static final int EOF=-1;

    // delegates
    // delegators


        public Test1Parser(TokenStream input) {
            this(input, new RecognizerSharedState());
        }
        public Test1Parser(TokenStream input, RecognizerSharedState state) {
            super(input, state);
             
        }
        

    public String[] getTokenNames() { return Test1Parser.tokenNames; }
    public String getGrammarFileName() { return "Test1.g"; }



    // $ANTLR start "head"
    // Test1.g:3:1: head : ( . )* WORD ( . )* ;
    public final void head() throws RecognitionException {
        try {
            // Test1.g:3:6: ( ( . )* WORD ( . )* )
            // Test1.g:3:8: ( . )* WORD ( . )*
            {
            // Test1.g:3:8: ( . )*
            loop1:
            do {
                int alt1=2;
                int LA1_0 = input.LA(1);

                if ( (LA1_0==WORD) ) {
                    alt1=2;
                }
                else if ( (LA1_0==ID) ) {
                    alt1=1;
                }


                switch (alt1) {
            	case 1 :
            	    // Test1.g:3:8: .
            	    {
            	    matchAny(input); 

            	    }
            	    break;

            	default :
            	    break loop1;
                }
            } while (true);

            match(input,WORD,FOLLOW_WORD_in_head13); 
            System.out.println("" + input.LT(1));
            // Test1.g:3:56: ( . )*
            loop2:
            do {
                int alt2=2;
                int LA2_0 = input.LA(1);

                if ( ((LA2_0>=WORD && LA2_0<=ID)) ) {
                    alt2=1;
                }
                else if ( (LA2_0==EOF) ) {
                    alt2=2;
                }


                switch (alt2) {
            	case 1 :
            	    // Test1.g:3:56: .
            	    {
            	    matchAny(input); 

            	    }
            	    break;

            	default :
            	    break loop2;
                }
            } while (true);


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
    // $ANTLR end "head"

    // Delegated rules


 

    public static final BitSet FOLLOW_WORD_in_head13 = new BitSet(new long[]{0x0000000000000032L});

}