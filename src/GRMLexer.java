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
		START_PARSER=1, START_LEXER=2, START_SKIP=3, ID=4, ParserID=5, LexerID=6, 
		JID=7, LCB=8, RCB=9, LB=10, RB=11, COLON=12, SEMICOLON=13, OR=14, COMMENT=15, 
		WS=16, TOKEN=17;
	public static String[] modeNames = {
		"DEFAULT_MODE"
	};

	public static final String[] tokenNames = {
		"<INVALID>",
		"'_PARSER'", "'_LEXER'", "'_SKIP'", "ID", "ParserID", "LexerID", "JID", 
		"'{'", "'}'", "'['", "']'", "':'", "';'", "'|'", "COMMENT", "WS", "TOKEN"
	};
	public static final String[] ruleNames = {
		"START_PARSER", "START_LEXER", "START_SKIP", "Letter", "SmallLetter", 
		"BigLetter", "Digit", "ID", "ParserID", "LexerID", "JID", "LCB", "RCB", 
		"LB", "RB", "COLON", "SEMICOLON", "OR", "COMMENT", "WS", "TOKEN"
	};

	 

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
		case 19: WS_action((RuleContext)_localctx, actionIndex); break;
		}
	}
	private void WS_action(RuleContext _localctx, int actionIndex) {
		switch (actionIndex) {
		case 0: skip();  break;
		}
	}

	public static final String _serializedATN =
		"\2\4\23\u008b\b\1\4\2\t\2\4\3\t\3\4\4\t\4\4\5\t\5\4\6\t\6\4\7\t\7\4\b"+
		"\t\b\4\t\t\t\4\n\t\n\4\13\t\13\4\f\t\f\4\r\t\r\4\16\t\16\4\17\t\17\4\20"+
		"\t\20\4\21\t\21\4\22\t\22\4\23\t\23\4\24\t\24\4\25\t\25\4\26\t\26\3\2"+
		"\3\2\3\2\3\2\3\2\3\2\3\2\3\2\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\4\3\4\3\4\3"+
		"\4\3\4\3\4\3\5\3\5\3\6\3\6\3\7\3\7\3\b\3\b\3\t\6\tL\n\t\r\t\16\tM\3\n"+
		"\3\n\7\nR\n\n\f\n\16\nU\13\n\3\13\3\13\7\13Y\n\13\f\13\16\13\\\13\13\3"+
		"\f\3\f\3\f\7\fa\n\f\f\f\16\fd\13\f\3\r\3\r\3\16\3\16\3\17\3\17\3\20\3"+
		"\20\3\21\3\21\3\22\3\22\3\23\3\23\3\24\3\24\7\24v\n\24\f\24\16\24y\13"+
		"\24\3\24\3\24\3\25\6\25~\n\25\r\25\16\25\177\3\25\3\25\3\26\3\26\6\26"+
		"\u0086\n\26\r\26\16\26\u0087\3\26\3\26\2\27\3\3\1\5\4\1\7\5\1\t\2\1\13"+
		"\2\1\r\2\1\17\2\1\21\6\1\23\7\1\25\b\1\27\t\1\31\n\1\33\13\1\35\f\1\37"+
		"\r\1!\16\1#\17\1%\20\1\'\21\1)\22\2+\23\1\3\2\6\5C\\aac|\4\f\f\17\17\5"+
		"\13\f\17\17\"\"\3$$\u008e\2\3\3\2\2\2\2\5\3\2\2\2\2\7\3\2\2\2\2\21\3\2"+
		"\2\2\2\23\3\2\2\2\2\25\3\2\2\2\2\27\3\2\2\2\2\31\3\2\2\2\2\33\3\2\2\2"+
		"\2\35\3\2\2\2\2\37\3\2\2\2\2!\3\2\2\2\2#\3\2\2\2\2%\3\2\2\2\2\'\3\2\2"+
		"\2\2)\3\2\2\2\2+\3\2\2\2\3-\3\2\2\2\5\65\3\2\2\2\7<\3\2\2\2\tB\3\2\2\2"+
		"\13D\3\2\2\2\rF\3\2\2\2\17H\3\2\2\2\21K\3\2\2\2\23O\3\2\2\2\25V\3\2\2"+
		"\2\27]\3\2\2\2\31e\3\2\2\2\33g\3\2\2\2\35i\3\2\2\2\37k\3\2\2\2!m\3\2\2"+
		"\2#o\3\2\2\2%q\3\2\2\2\'s\3\2\2\2)}\3\2\2\2+\u0083\3\2\2\2-.\7a\2\2./"+
		"\7R\2\2/\60\7C\2\2\60\61\7T\2\2\61\62\7U\2\2\62\63\7G\2\2\63\64\7T\2\2"+
		"\64\4\3\2\2\2\65\66\7a\2\2\66\67\7N\2\2\678\7G\2\289\7Z\2\29:\7G\2\2:"+
		";\7T\2\2;\6\3\2\2\2<=\7a\2\2=>\7U\2\2>?\7M\2\2?@\7K\2\2@A\7R\2\2A\b\3"+
		"\2\2\2BC\t\2\2\2C\n\3\2\2\2DE\4c|\2E\f\3\2\2\2FG\4C\\\2G\16\3\2\2\2HI"+
		"\4\62;\2I\20\3\2\2\2JL\5\t\5\2KJ\3\2\2\2LM\3\2\2\2MK\3\2\2\2MN\3\2\2\2"+
		"N\22\3\2\2\2OS\5\13\6\2PR\5\t\5\2QP\3\2\2\2RU\3\2\2\2SQ\3\2\2\2ST\3\2"+
		"\2\2T\24\3\2\2\2US\3\2\2\2VZ\5\r\7\2WY\5\t\5\2XW\3\2\2\2Y\\\3\2\2\2ZX"+
		"\3\2\2\2Z[\3\2\2\2[\26\3\2\2\2\\Z\3\2\2\2]b\5\t\5\2^a\5\t\5\2_a\5\17\b"+
		"\2`^\3\2\2\2`_\3\2\2\2ad\3\2\2\2b`\3\2\2\2bc\3\2\2\2c\30\3\2\2\2db\3\2"+
		"\2\2ef\7}\2\2f\32\3\2\2\2gh\7\177\2\2h\34\3\2\2\2ij\7]\2\2j\36\3\2\2\2"+
		"kl\7_\2\2l \3\2\2\2mn\7<\2\2n\"\3\2\2\2op\7=\2\2p$\3\2\2\2qr\7~\2\2r&"+
		"\3\2\2\2sw\7%\2\2tv\n\3\2\2ut\3\2\2\2vy\3\2\2\2wu\3\2\2\2wx\3\2\2\2xz"+
		"\3\2\2\2yw\3\2\2\2z{\7\f\2\2{(\3\2\2\2|~\t\4\2\2}|\3\2\2\2~\177\3\2\2"+
		"\2\177}\3\2\2\2\177\u0080\3\2\2\2\u0080\u0081\3\2\2\2\u0081\u0082\b\25"+
		"\2\2\u0082*\3\2\2\2\u0083\u0085\7$\2\2\u0084\u0086\n\5\2\2\u0085\u0084"+
		"\3\2\2\2\u0086\u0087\3\2\2\2\u0087\u0085\3\2\2\2\u0087\u0088\3\2\2\2\u0088"+
		"\u0089\3\2\2\2\u0089\u008a\7$\2\2\u008a,\3\2\2\2\13\2MSZ`bw\177\u0087";
	public static final ATN _ATN =
		ATNSimulator.deserialize(_serializedATN.toCharArray());
	static {
		_decisionToDFA = new DFA[_ATN.getNumberOfDecisions()];
	}
}