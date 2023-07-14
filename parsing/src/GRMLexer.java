// Generated from GRM.g4 by ANTLR 4.0

import java.util.*;
import java.io.*;

import org.antlr.v4.runtime.Lexer;
import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.Token;
import org.antlr.v4.runtime.TokenStream;
import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.atn.*;
import org.antlr.v4.runtime.dfa.DFA;
import org.antlr.v4.runtime.misc.*;

@SuppressWarnings({"all", "warnings", "unchecked", "unused", "cast"})
public class GRMLexer extends Lexer {
	protected static final DFA[] _decisionToDFA;
	protected static final PredictionContextCache _sharedContextCache =
		new PredictionContextCache();
	public static final int
		START_IMPORT=1, START_PARSER=2, START_LEXER=3, START_SKIP=4, ArgVar=5, 
		RetVar=6, JCode=7, CallArgs=8, ParserID=9, LexerID=10, ID=11, COLON=12, 
		SEMICOLON=13, OR=14, COMMENT=15, WS=16, TOKEN=17;
	public static String[] modeNames = {
		"DEFAULT_MODE"
	};

	public static final String[] tokenNames = {
		"<INVALID>",
		"'_IMPORT'", "'_PARSER'", "'_LEXER'", "'_SKIP'", "ArgVar", "RetVar", "JCode", 
		"CallArgs", "ParserID", "LexerID", "ID", "':'", "';'", "'|'", "COMMENT", 
		"WS", "TOKEN"
	};
	public static final String[] ruleNames = {
		"START_IMPORT", "START_PARSER", "START_LEXER", "START_SKIP", "ArgVar", 
		"RetVar", "JCode", "CallArgs", "SmallLetter", "BigLetter", "Letter", "Digit", 
		"NL", "ParserID", "LexerID", "ID", "JID", "COLON", "SEMICOLON", "OR", 
		"COMMENT", "WS", "TOKEN"
	};


	    String crop(String x, int n) {
	        return x.substring(n, x.length() - n);
	    }


	public GRMLexer(CharStream input) {
		super(input);
		_interp = new LexerATNSimulator(this,_ATN,_decisionToDFA,_sharedContextCache);
	}

	@Override
	public String getGrammarFileName() { return "GRM.g4"; }

	@Override
	public String[] getTokenNames() { return tokenNames; }

	@Override
	public String[] getRuleNames() { return ruleNames; }

	@Override
	public String[] getModeNames() { return modeNames; }

	@Override
	public ATN getATN() { return _ATN; }

	@Override
	public void action(RuleContext _localctx, int ruleIndex, int actionIndex) {
		switch (ruleIndex) {
		case 21: WS_action((RuleContext)_localctx, actionIndex); break;
		}
	}
	private void WS_action(RuleContext _localctx, int actionIndex) {
		switch (actionIndex) {
		case 0: skip();  break;
		}
	}

	public static final String _serializedATN =
		"\2\4\23\u00bb\b\1\4\2\t\2\4\3\t\3\4\4\t\4\4\5\t\5\4\6\t\6\4\7\t\7\4\b"+
		"\t\b\4\t\t\t\4\n\t\n\4\13\t\13\4\f\t\f\4\r\t\r\4\16\t\16\4\17\t\17\4\20"+
		"\t\20\4\21\t\21\4\22\t\22\4\23\t\23\4\24\t\24\4\25\t\25\4\26\t\26\4\27"+
		"\t\27\4\30\t\30\3\2\3\2\3\2\3\2\3\2\3\2\3\2\3\2\3\3\3\3\3\3\3\3\3\3\3"+
		"\3\3\3\3\3\3\4\3\4\3\4\3\4\3\4\3\4\3\4\3\5\3\5\3\5\3\5\3\5\3\5\3\6\3\6"+
		"\6\6Q\n\6\r\6\16\6R\3\6\3\6\3\7\3\7\6\7Y\n\7\r\7\16\7Z\3\7\3\7\3\b\3\b"+
		"\3\b\3\b\3\b\7\bd\n\b\f\b\16\bg\13\b\3\b\3\b\3\b\3\t\3\t\3\t\3\t\7\tp"+
		"\n\t\f\t\16\ts\13\t\3\t\3\t\3\t\3\n\3\n\3\13\3\13\3\f\3\f\3\r\3\r\3\16"+
		"\3\16\3\17\3\17\7\17\u0084\n\17\f\17\16\17\u0087\13\17\3\20\3\20\7\20"+
		"\u008b\n\20\f\20\16\20\u008e\13\20\3\21\6\21\u0091\n\21\r\21\16\21\u0092"+
		"\3\22\3\22\3\22\3\22\7\22\u0099\n\22\f\22\16\22\u009c\13\22\3\23\3\23"+
		"\3\24\3\24\3\25\3\25\3\26\3\26\7\26\u00a6\n\26\f\26\16\26\u00a9\13\26"+
		"\3\26\3\26\3\27\6\27\u00ae\n\27\r\27\16\27\u00af\3\27\3\27\3\30\3\30\6"+
		"\30\u00b6\n\30\r\30\16\30\u00b7\3\30\3\30\4eq\31\3\3\1\5\4\1\7\5\1\t\6"+
		"\1\13\7\1\r\b\1\17\t\1\21\n\1\23\2\1\25\2\1\27\2\1\31\2\1\33\2\1\35\13"+
		"\1\37\f\1!\r\1#\2\1%\16\1\'\17\1)\20\1+\21\1-\22\2/\23\1\3\2\b\3\f\f\3"+
		"\f\f\5C\\aac|\4\f\f\17\17\5\13\f\17\17\"\"\3$$\u00c2\2\3\3\2\2\2\2\5\3"+
		"\2\2\2\2\7\3\2\2\2\2\t\3\2\2\2\2\13\3\2\2\2\2\r\3\2\2\2\2\17\3\2\2\2\2"+
		"\21\3\2\2\2\2\35\3\2\2\2\2\37\3\2\2\2\2!\3\2\2\2\2%\3\2\2\2\2\'\3\2\2"+
		"\2\2)\3\2\2\2\2+\3\2\2\2\2-\3\2\2\2\2/\3\2\2\2\3\61\3\2\2\2\59\3\2\2\2"+
		"\7A\3\2\2\2\tH\3\2\2\2\13N\3\2\2\2\rV\3\2\2\2\17^\3\2\2\2\21k\3\2\2\2"+
		"\23w\3\2\2\2\25y\3\2\2\2\27{\3\2\2\2\31}\3\2\2\2\33\177\3\2\2\2\35\u0081"+
		"\3\2\2\2\37\u0088\3\2\2\2!\u0090\3\2\2\2#\u0094\3\2\2\2%\u009d\3\2\2\2"+
		"\'\u009f\3\2\2\2)\u00a1\3\2\2\2+\u00a3\3\2\2\2-\u00ad\3\2\2\2/\u00b3\3"+
		"\2\2\2\61\62\7a\2\2\62\63\7K\2\2\63\64\7O\2\2\64\65\7R\2\2\65\66\7Q\2"+
		"\2\66\67\7T\2\2\678\7V\2\28\4\3\2\2\29:\7a\2\2:;\7R\2\2;<\7C\2\2<=\7T"+
		"\2\2=>\7U\2\2>?\7G\2\2?@\7T\2\2@\6\3\2\2\2AB\7a\2\2BC\7N\2\2CD\7G\2\2"+
		"DE\7Z\2\2EF\7G\2\2FG\7T\2\2G\b\3\2\2\2HI\7a\2\2IJ\7U\2\2JK\7M\2\2KL\7"+
		"K\2\2LM\7R\2\2M\n\3\2\2\2NP\7\'\2\2OQ\n\2\2\2PO\3\2\2\2QR\3\2\2\2RP\3"+
		"\2\2\2RS\3\2\2\2ST\3\2\2\2TU\5\33\16\2U\f\3\2\2\2VX\7`\2\2WY\n\3\2\2X"+
		"W\3\2\2\2YZ\3\2\2\2ZX\3\2\2\2Z[\3\2\2\2[\\\3\2\2\2\\]\5\33\16\2]\16\3"+
		"\2\2\2^_\7}\2\2_`\7&\2\2`e\3\2\2\2ad\5/\30\2bd\13\2\2\2ca\3\2\2\2cb\3"+
		"\2\2\2dg\3\2\2\2ef\3\2\2\2ec\3\2\2\2fh\3\2\2\2ge\3\2\2\2hi\7&\2\2ij\7"+
		"\177\2\2j\20\3\2\2\2kl\7&\2\2lm\7*\2\2mq\3\2\2\2np\13\2\2\2on\3\2\2\2"+
		"ps\3\2\2\2qr\3\2\2\2qo\3\2\2\2rt\3\2\2\2sq\3\2\2\2tu\7+\2\2uv\7&\2\2v"+
		"\22\3\2\2\2wx\4c|\2x\24\3\2\2\2yz\4C\\\2z\26\3\2\2\2{|\t\4\2\2|\30\3\2"+
		"\2\2}~\4\62;\2~\32\3\2\2\2\177\u0080\7\f\2\2\u0080\34\3\2\2\2\u0081\u0085"+
		"\5\23\n\2\u0082\u0084\5\27\f\2\u0083\u0082\3\2\2\2\u0084\u0087\3\2\2\2"+
		"\u0085\u0083\3\2\2\2\u0085\u0086\3\2\2\2\u0086\36\3\2\2\2\u0087\u0085"+
		"\3\2\2\2\u0088\u008c\5\25\13\2\u0089\u008b\5\27\f\2\u008a\u0089\3\2\2"+
		"\2\u008b\u008e\3\2\2\2\u008c\u008a\3\2\2\2\u008c\u008d\3\2\2\2\u008d "+
		"\3\2\2\2\u008e\u008c\3\2\2\2\u008f\u0091\5\27\f\2\u0090\u008f\3\2\2\2"+
		"\u0091\u0092\3\2\2\2\u0092\u0090\3\2\2\2\u0092\u0093\3\2\2\2\u0093\"\3"+
		"\2\2\2\u0094\u009a\5\27\f\2\u0095\u0099\5\27\f\2\u0096\u0099\5\31\r\2"+
		"\u0097\u0099\7\60\2\2\u0098\u0095\3\2\2\2\u0098\u0096\3\2\2\2\u0098\u0097"+
		"\3\2\2\2\u0099\u009c\3\2\2\2\u009a\u0098\3\2\2\2\u009a\u009b\3\2\2\2\u009b"+
		"$\3\2\2\2\u009c\u009a\3\2\2\2\u009d\u009e\7<\2\2\u009e&\3\2\2\2\u009f"+
		"\u00a0\7=\2\2\u00a0(\3\2\2\2\u00a1\u00a2\7~\2\2\u00a2*\3\2\2\2\u00a3\u00a7"+
		"\7%\2\2\u00a4\u00a6\n\5\2\2\u00a5\u00a4\3\2\2\2\u00a6\u00a9\3\2\2\2\u00a7"+
		"\u00a5\3\2\2\2\u00a7\u00a8\3\2\2\2\u00a8\u00aa\3\2\2\2\u00a9\u00a7\3\2"+
		"\2\2\u00aa\u00ab\7\f\2\2\u00ab,\3\2\2\2\u00ac\u00ae\t\6\2\2\u00ad\u00ac"+
		"\3\2\2\2\u00ae\u00af\3\2\2\2\u00af\u00ad\3\2\2\2\u00af\u00b0\3\2\2\2\u00b0"+
		"\u00b1\3\2\2\2\u00b1\u00b2\b\27\2\2\u00b2.\3\2\2\2\u00b3\u00b5\7$\2\2"+
		"\u00b4\u00b6\n\7\2\2\u00b5\u00b4\3\2\2\2\u00b6\u00b7\3\2\2\2\u00b7\u00b5"+
		"\3\2\2\2\u00b7\u00b8\3\2\2\2\u00b8\u00b9\3\2\2\2\u00b9\u00ba\7$\2\2\u00ba"+
		"\60\3\2\2\2\20\2RZceq\u0085\u008c\u0092\u0098\u009a\u00a7\u00af\u00b7";
	public static final ATN _ATN =
		ATNSimulator.deserialize(_serializedATN.toCharArray());
	static {
		_decisionToDFA = new DFA[_ATN.getNumberOfDecisions()];
	}
}