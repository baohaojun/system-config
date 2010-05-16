import org.antlr.runtime.CommonTokenStream;
import org.antlr.runtime.*;
import org.antlr.runtime.ANTLRFileStream;
import org.antlr.runtime.debug.BlankDebugEventListener;

import java.io.File;

/** Parse a java file or directory of java files using the generated parser
 *  ANTLR builds from java.g
 */
class BeagleBreakTokens {
	public static long lexerTime = 0;
	public static boolean profile = false;

	static JavaLexer lexer;

	public static void main(String[] args) {
		try {
			long start = System.currentTimeMillis();
			if (args.length > 0 ) {
				// for each directory/file specified on the command line
				for(int i=0; i< args.length;i++) {
					doFile(new File(args[i])); // parse it
				}
			}
			else {
				System.err.println("Usage: java Main <directory or file name>");
			}
			long stop = System.currentTimeMillis();

			if ( profile ) {
				System.out.println("num decisions "+profiler.numDecisions);
			}
		}
		catch(Exception e) {
			System.err.println("exception: "+e);
			e.printStackTrace(System.err);   // so we can get stack trace
		}
	}


	// This method decides what action to take based on the type of
	//   file we are looking at
	public static void doFile(File f)
							  throws Exception {
		// If this is a directory, walk each file/dir in that directory
		if (f.isDirectory()) {
			String files[] = f.list();
			for(int i=0; i < files.length; i++)
				doFile(new File(f, files[i]));
		}

		// otherwise, if this is a java file, parse it!
		else if ( ((f.getName().length()>5) &&
				f.getName().substring(f.getName().length()-5).equals(".java"))
			|| f.getName().equals("input") )
		{
			parseFile(f.getAbsolutePath());
		}
	}

	static class CountDecisions extends BlankDebugEventListener {
		public int numDecisions = 0;
		public void enterDecision(int decisionNumber) {
			numDecisions++;
		}
	}
	static CountDecisions profiler = new CountDecisions();

	// Here's where we do the real work...
	public static void parseFile(String f)
								 throws Exception {
		try {
			// Create a scanner that reads from the input stream passed to us
			if ( lexer==null ) {
				lexer = new BeagleTokens();
			}
			lexer.setCharStream(new ANTLRFileStream(f));
			CommonTokenStream tokens = new CommonTokenStream();
//			tokens.discardOffChannelTokens(true);
			tokens.setTokenSource(lexer);
			long start = System.currentTimeMillis();
			tokens.LT(1); // force load
			long stop = System.currentTimeMillis();
			lexerTime += stop-start;

			/*
			long t1 = System.currentTimeMillis();
			tokens.LT(1);
			long t2 = System.currentTimeMillis();
			System.out.println("lexing time: "+(t2-t1)+"ms");
			*/
			//System.out.println(tokens);

			// Create a parser that reads from the scanner
                        boolean lastNL = true; 
                        while (true) {
                            Token t = tokens.LT(1);
                            if (t == Token.EOF_TOKEN) {
                                break;
                            } else if (t.getType() == JavaParser.COMMENT || t.getType() == JavaParser.LINE_COMMENT) {
                                if (! lastNL) {
                                    System.out.println("");
                                    lastNL = true;
                                }
                            } else {
                                System.out.print(t.getText());
                                if (t.getText().contains("\n")){
                                    lastNL = true;
                                } else {
                                    lastNL = false;
                                }
                            }
                            tokens.consume();
                        }
		}
		catch (Exception e) {
			System.err.println("parser exception: "+e);
			e.printStackTrace();   // so we can get stack trace		
		}
	}
	
}

