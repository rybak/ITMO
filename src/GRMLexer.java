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
		START_PARSER=1, START_LEXER=2, START_SKIP=3, VARS_B=4, JVar=5, JCode=6, 
		ParserID=7, LexerID=8, ID=9, LCB=10, RCB=11, LB=12, RB=13, COLON=14, SEMICOLON=15, 
		OR=16, COMMENT=17, WS=18, TOKEN=19;
	public static String[] modeNames = {
		"DEFAULT_MODE"
	};

	public static final String[] tokenNames = {
		"<INVALID>",
		"'_PARSER'", "'_LEXER'", "'_SKIP'", "VARS_B", "JVar", "JCode", "ParserID", 
		"LexerID", "ID", "'{\n'", "'}\n'", "'[\n'", "']\n'", "':'", "';'", "'|'", 
		"COMMENT", "WS", "TOKEN"
	};
	public static final String[] ruleNames = {
		"START_PARSER", "START_LEXER", "START_SKIP", "SMark", "VARS_B", "JVar", 
		"JMark", "JCode", "SmallLetter", "BigLetter", "Letter", "Digit", "NL", 
		"ParserID", "LexerID", "ID", "JID", "LCB", "RCB", "LB", "RB", "COLON", 
		"SEMICOLON", "OR", "COMMENT", "WS", "TOKEN"
	};


	    String crop(String x) {
	        return x.substring(1, x.length() - 1);
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
		case 25: WS_action((RuleContext)_localctx, actionIndex); break;
		}
	}
	private void WS_action(RuleContext _localctx, int actionIndex) {
		switch (actionIndex) {
		case 0: skip();  break;
		}
	}

	public static final String _serializedATN =
		"\2\4\25\u00b8\b\1\4\2\t\2\4\3\t\3\4\4\t\4\4\5\t\5\4\6\t\6\4\7\t\7\4\b"+
		"\t\b\4\t\t\t\4\n\t\n\4\13\t\13\4\f\t\f\4\r\t\r\4\16\t\16\4\17\t\17\4\20"+
		"\t\20\4\21\t\21\4\22\t\22\4\23\t\23\4\24\t\24\4\25\t\25\4\26\t\26\4\27"+
		"\t\27\4\30\t\30\4\31\t\31\4\32\t\32\4\33\t\33\4\34\t\34\3\2\3\2\3\2\3"+
		"\2\3\2\3\2\3\2\3\2\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\4\3\4\3\4\3\4\3\4\3\4"+
		"\3\5\3\5\3\6\3\6\3\6\3\6\3\7\3\7\7\7W\n\7\f\7\16\7Z\13\7\3\7\3\7\3\b\3"+
		"\b\3\t\3\t\7\tb\n\t\f\t\16\te\13\t\3\t\3\t\3\n\3\n\3\13\3\13\3\f\3\f\3"+
		"\r\3\r\3\16\3\16\3\17\3\17\7\17u\n\17\f\17\16\17x\13\17\3\20\3\20\7\20"+
		"|\n\20\f\20\16\20\177\13\20\3\21\6\21\u0082\n\21\r\21\16\21\u0083\3\22"+
		"\3\22\3\22\3\22\7\22\u008a\n\22\f\22\16\22\u008d\13\22\3\23\3\23\3\23"+
		"\3\24\3\24\3\24\3\25\3\25\3\25\3\26\3\26\3\26\3\27\3\27\3\30\3\30\3\31"+
		"\3\31\3\32\3\32\7\32\u00a3\n\32\f\32\16\32\u00a6\13\32\3\32\3\32\3\33"+
		"\6\33\u00ab\n\33\r\33\16\33\u00ac\3\33\3\33\3\34\3\34\6\34\u00b3\n\34"+
		"\r\34\16\34\u00b4\3\34\3\34\2\35\3\3\1\5\4\1\7\5\1\t\2\1\13\6\1\r\7\1"+
		"\17\2\1\21\b\1\23\2\1\25\2\1\27\2\1\31\2\1\33\2\1\35\t\1\37\n\1!\13\1"+
		"#\2\1%\f\1\'\r\1)\16\1+\17\1-\20\1/\21\1\61\22\1\63\23\1\65\24\2\67\25"+
		"\1\3\2\b\3\f\f\3\f\f\5C\\aac|\4\f\f\17\17\5\13\f\17\17\"\"\3$$\u00ba\2"+
		"\3\3\2\2\2\2\5\3\2\2\2\2\7\3\2\2\2\2\13\3\2\2\2\2\r\3\2\2\2\2\21\3\2\2"+
		"\2\2\35\3\2\2\2\2\37\3\2\2\2\2!\3\2\2\2\2%\3\2\2\2\2\'\3\2\2\2\2)\3\2"+
		"\2\2\2+\3\2\2\2\2-\3\2\2\2\2/\3\2\2\2\2\61\3\2\2\2\2\63\3\2\2\2\2\65\3"+
		"\2\2\2\2\67\3\2\2\2\39\3\2\2\2\5A\3\2\2\2\7H\3\2\2\2\tN\3\2\2\2\13P\3"+
		"\2\2\2\rT\3\2\2\2\17]\3\2\2\2\21_\3\2\2\2\23h\3\2\2\2\25j\3\2\2\2\27l"+
		"\3\2\2\2\31n\3\2\2\2\33p\3\2\2\2\35r\3\2\2\2\37y\3\2\2\2!\u0081\3\2\2"+
		"\2#\u0085\3\2\2\2%\u008e\3\2\2\2\'\u0091\3\2\2\2)\u0094\3\2\2\2+\u0097"+
		"\3\2\2\2-\u009a\3\2\2\2/\u009c\3\2\2\2\61\u009e\3\2\2\2\63\u00a0\3\2\2"+
		"\2\65\u00aa\3\2\2\2\67\u00b0\3\2\2\29:\7a\2\2:;\7R\2\2;<\7C\2\2<=\7T\2"+
		"\2=>\7U\2\2>?\7G\2\2?@\7T\2\2@\4\3\2\2\2AB\7a\2\2BC\7N\2\2CD\7G\2\2DE"+
		"\7Z\2\2EF\7G\2\2FG\7T\2\2G\6\3\2\2\2HI\7a\2\2IJ\7U\2\2JK\7M\2\2KL\7K\2"+
		"\2LM\7R\2\2M\b\3\2\2\2NO\7\'\2\2O\n\3\2\2\2PQ\5\t\5\2QR\7]\2\2RS\5\33"+
		"\16\2S\f\3\2\2\2TX\7\'\2\2UW\n\2\2\2VU\3\2\2\2WZ\3\2\2\2XV\3\2\2\2XY\3"+
		"\2\2\2Y[\3\2\2\2ZX\3\2\2\2[\\\5\33\16\2\\\16\3\2\2\2]^\7%\2\2^\20\3\2"+
		"\2\2_c\5\17\b\2`b\n\3\2\2a`\3\2\2\2be\3\2\2\2ca\3\2\2\2cd\3\2\2\2df\3"+
		"\2\2\2ec\3\2\2\2fg\5\33\16\2g\22\3\2\2\2hi\4c|\2i\24\3\2\2\2jk\4C\\\2"+
		"k\26\3\2\2\2lm\t\4\2\2m\30\3\2\2\2no\4\62;\2o\32\3\2\2\2pq\7\f\2\2q\34"+
		"\3\2\2\2rv\5\23\n\2su\5\27\f\2ts\3\2\2\2ux\3\2\2\2vt\3\2\2\2vw\3\2\2\2"+
		"w\36\3\2\2\2xv\3\2\2\2y}\5\25\13\2z|\5\27\f\2{z\3\2\2\2|\177\3\2\2\2}"+
		"{\3\2\2\2}~\3\2\2\2~ \3\2\2\2\177}\3\2\2\2\u0080\u0082\5\27\f\2\u0081"+
		"\u0080\3\2\2\2\u0082\u0083\3\2\2\2\u0083\u0081\3\2\2\2\u0083\u0084\3\2"+
		"\2\2\u0084\"\3\2\2\2\u0085\u008b\5\27\f\2\u0086\u008a\5\27\f\2\u0087\u008a"+
		"\5\31\r\2\u0088\u008a\7\60\2\2\u0089\u0086\3\2\2\2\u0089\u0087\3\2\2\2"+
		"\u0089\u0088\3\2\2\2\u008a\u008d\3\2\2\2\u008b\u0089\3\2\2\2\u008b\u008c"+
		"\3\2\2\2\u008c$\3\2\2\2\u008d\u008b\3\2\2\2\u008e\u008f\7}\2\2\u008f\u0090"+
		"\7\f\2\2\u0090&\3\2\2\2\u0091\u0092\7\177\2\2\u0092\u0093\7\f\2\2\u0093"+
		"(\3\2\2\2\u0094\u0095\7]\2\2\u0095\u0096\7\f\2\2\u0096*\3\2\2\2\u0097"+
		"\u0098\7_\2\2\u0098\u0099\7\f\2\2\u0099,\3\2\2\2\u009a\u009b\7<\2\2\u009b"+
		".\3\2\2\2\u009c\u009d\7=\2\2\u009d\60\3\2\2\2\u009e\u009f\7~\2\2\u009f"+
		"\62\3\2\2\2\u00a0\u00a4\7%\2\2\u00a1\u00a3\n\5\2\2\u00a2\u00a1\3\2\2\2"+
		"\u00a3\u00a6\3\2\2\2\u00a4\u00a2\3\2\2\2\u00a4\u00a5\3\2\2\2\u00a5\u00a7"+
		"\3\2\2\2\u00a6\u00a4\3\2\2\2\u00a7\u00a8\7\f\2\2\u00a8\64\3\2\2\2\u00a9"+
		"\u00ab\t\6\2\2\u00aa\u00a9\3\2\2\2\u00ab\u00ac\3\2\2\2\u00ac\u00aa\3\2"+
		"\2\2\u00ac\u00ad\3\2\2\2\u00ad\u00ae\3\2\2\2\u00ae\u00af\b\33\2\2\u00af"+
		"\66\3\2\2\2\u00b0\u00b2\7$\2\2\u00b1\u00b3\n\7\2\2\u00b2\u00b1\3\2\2\2"+
		"\u00b3\u00b4\3\2\2\2\u00b4\u00b2\3\2\2\2\u00b4\u00b5\3\2\2\2\u00b5\u00b6"+
		"\3\2\2\2\u00b6\u00b7\7$\2\2\u00b78\3\2\2\2\r\2Xcv}\u0083\u0089\u008b\u00a4"+
		"\u00ac\u00b4";
	public static final ATN _ATN =
		ATNSimulator.deserialize(_serializedATN.toCharArray());
	static {
		_decisionToDFA = new DFA[_ATN.getNumberOfDecisions()];
	}
}