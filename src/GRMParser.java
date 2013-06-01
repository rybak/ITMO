// Generated from GRM.g4 by ANTLR 4.0

import java.util.*;
import java.io.*;

import org.antlr.v4.runtime.atn.*;
import org.antlr.v4.runtime.dfa.DFA;
import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.misc.*;
import org.antlr.v4.runtime.tree.*;
import java.util.List;
import java.util.Iterator;
import java.util.ArrayList;

@SuppressWarnings({"all", "warnings", "unchecked", "unused", "cast"})
public class GRMParser extends Parser {
	protected static final DFA[] _decisionToDFA;
	protected static final PredictionContextCache _sharedContextCache =
		new PredictionContextCache();
	public static final int
		START_PARSER=1, START_LEXER=2, START_SKIP=3, ID=4, ParserID=5, LexerID=6, 
		JID=7, LCB=8, RCB=9, LB=10, RB=11, COLON=12, SEMICOLON=13, OR=14, COMMENT=15, 
		WS=16, TOKEN=17;
	public static final String[] tokenNames = {
		"<INVALID>", "'_PARSER'", "'_LEXER'", "'_SKIP'", "ID", "ParserID", "LexerID", 
		"JID", "'{'", "'}'", "'['", "']'", "':'", "';'", "'|'", "COMMENT", "WS", 
		"TOKEN"
	};
	public static final int
		RULE_file = 0, RULE_parsing = 1, RULE_args = 2, RULE_parseExpr = 3, RULE_lexerRule = 4, 
		RULE_token = 5;
	public static final String[] ruleNames = {
		"file", "parsing", "args", "parseExpr", "lexerRule", "token"
	};

	@Override
	public String getGrammarFileName() { return "GRM.g4"; }

	@Override
	public String[] getTokenNames() { return tokenNames; }

	@Override
	public String[] getRuleNames() { return ruleNames; }

	@Override
	public ATN getATN() { return _ATN; }

	 
	public GRMParser(TokenStream input) {
		super(input);
		_interp = new ParserATNSimulator(this,_ATN,_decisionToDFA,_sharedContextCache);
	}
	public static class FileContext extends ParserRuleContext {
		public String name;
		public ArrayList<LexerRule> lexerRules;
		public ArrayList<LexerRule> skipRules;
		public Token ID;
		public LexerRuleContext lexerRule;
		public TerminalNode START_PARSER() { return getToken(GRMParser.START_PARSER, 0); }
		public TerminalNode ID() { return getToken(GRMParser.ID, 0); }
		public List<ParsingContext> parsing() {
			return getRuleContexts(ParsingContext.class);
		}
		public TerminalNode EOF() { return getToken(GRMParser.EOF, 0); }
		public List<LexerRuleContext> lexerRule() {
			return getRuleContexts(LexerRuleContext.class);
		}
		public TerminalNode START_LEXER() { return getToken(GRMParser.START_LEXER, 0); }
		public LexerRuleContext lexerRule(int i) {
			return getRuleContext(LexerRuleContext.class,i);
		}
		public TerminalNode START_SKIP() { return getToken(GRMParser.START_SKIP, 0); }
		public ParsingContext parsing(int i) {
			return getRuleContext(ParsingContext.class,i);
		}
		public FileContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_file; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof GRMListener ) ((GRMListener)listener).enterFile(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof GRMListener ) ((GRMListener)listener).exitFile(this);
		}
	}

	public final FileContext file() throws RecognitionException {
		FileContext _localctx = new FileContext(_ctx, getState());
		enterRule(_localctx, 0, RULE_file);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{

			        ((FileContext)_localctx).lexerRules =  new ArrayList<LexerRule>();
			        ((FileContext)_localctx).skipRules =  new ArrayList<LexerRule>();
			    
			setState(13); ((FileContext)_localctx).ID = match(ID);
			 ((FileContext)_localctx).name =  (((FileContext)_localctx).ID!=null?((FileContext)_localctx).ID.getText():null); 
			setState(15); match(START_PARSER);
			setState(17); 
			_errHandler.sync(this);
			_la = _input.LA(1);
			do {
				{
				{
				setState(16); parsing();
				}
				}
				setState(19); 
				_errHandler.sync(this);
				_la = _input.LA(1);
			} while ( _la==ParserID );
			setState(21); match(START_LEXER);
			setState(25); 
			_errHandler.sync(this);
			_la = _input.LA(1);
			do {
				{
				{
				setState(22); ((FileContext)_localctx).lexerRule = lexerRule();
				 _localctx.lexerRules.add(((FileContext)_localctx).lexerRule.r); 
				}
				}
				setState(27); 
				_errHandler.sync(this);
				_la = _input.LA(1);
			} while ( _la==LexerID );
			setState(37);
			_la = _input.LA(1);
			if (_la==START_SKIP) {
				{
				setState(29); match(START_SKIP);
				setState(33); 
				_errHandler.sync(this);
				_la = _input.LA(1);
				do {
					{
					{
					setState(30); ((FileContext)_localctx).lexerRule = lexerRule();
					 _localctx.skipRules.add(((FileContext)_localctx).lexerRule.r); 
					}
					}
					setState(35); 
					_errHandler.sync(this);
					_la = _input.LA(1);
				} while ( _la==LexerID );
				}
			}

			setState(39); match(EOF);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class ParsingContext extends ParserRuleContext {
		public ParserRule r;
		public Token ParserID;
		public ArgsContext args;
		public TerminalNode COLON() { return getToken(GRMParser.COLON, 0); }
		public ParseExprContext parseExpr() {
			return getRuleContext(ParseExprContext.class,0);
		}
		public TerminalNode ParserID() { return getToken(GRMParser.ParserID, 0); }
		public TerminalNode SEMICOLON() { return getToken(GRMParser.SEMICOLON, 0); }
		public ArgsContext args() {
			return getRuleContext(ArgsContext.class,0);
		}
		public ParsingContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_parsing; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof GRMListener ) ((GRMListener)listener).enterParsing(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof GRMListener ) ((GRMListener)listener).exitParsing(this);
		}
	}

	public final ParsingContext parsing() throws RecognitionException {
		ParsingContext _localctx = new ParsingContext(_ctx, getState());
		enterRule(_localctx, 2, RULE_parsing);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{

			        ArrayList<String> a = null;
			    
			setState(42); ((ParsingContext)_localctx).ParserID = match(ParserID);
			setState(46);
			_la = _input.LA(1);
			if (_la==LB) {
				{
				setState(43); ((ParsingContext)_localctx).args = args();

				            a = ((ParsingContext)_localctx).args.res;
				        
				}
			}

			setState(48); match(COLON);
			setState(49); parseExpr();
			setState(50); match(SEMICOLON);

			        ((ParsingContext)_localctx).r =  new ParserRule((((ParsingContext)_localctx).ParserID!=null?((ParsingContext)_localctx).ParserID.getText():null), a);
			    
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class ArgsContext extends ParserRuleContext {
		public ArrayList<String> res;
		public Token t;
		public Token n;
		public TerminalNode LB() { return getToken(GRMParser.LB, 0); }
		public List<TerminalNode> JID() { return getTokens(GRMParser.JID); }
		public TerminalNode RB() { return getToken(GRMParser.RB, 0); }
		public TerminalNode JID(int i) {
			return getToken(GRMParser.JID, i);
		}
		public ArgsContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_args; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof GRMListener ) ((GRMListener)listener).enterArgs(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof GRMListener ) ((GRMListener)listener).exitArgs(this);
		}
	}

	public final ArgsContext args() throws RecognitionException {
		ArgsContext _localctx = new ArgsContext(_ctx, getState());
		enterRule(_localctx, 4, RULE_args);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{

			        ((ArgsContext)_localctx).res =  new ArrayList<String>();
			    
			setState(54); match(LB);
			setState(58); 
			_errHandler.sync(this);
			_la = _input.LA(1);
			do {
				{
				{
				setState(55); ((ArgsContext)_localctx).t = match(JID);
				setState(56); ((ArgsContext)_localctx).n = match(JID);

				            _localctx.res.add((((ArgsContext)_localctx).t!=null?((ArgsContext)_localctx).t.getText():null) + ' ' + (((ArgsContext)_localctx).n!=null?((ArgsContext)_localctx).n.getText():null));
				        
				}
				}
				setState(60); 
				_errHandler.sync(this);
				_la = _input.LA(1);
			} while ( _la==JID );
			setState(62); match(RB);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class ParseExprContext extends ParserRuleContext {
		public TerminalNode ID(int i) {
			return getToken(GRMParser.ID, i);
		}
		public TerminalNode OR(int i) {
			return getToken(GRMParser.OR, i);
		}
		public List<TerminalNode> ID() { return getTokens(GRMParser.ID); }
		public List<TerminalNode> OR() { return getTokens(GRMParser.OR); }
		public ParseExprContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_parseExpr; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof GRMListener ) ((GRMListener)listener).enterParseExpr(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof GRMListener ) ((GRMListener)listener).exitParseExpr(this);
		}
	}

	public final ParseExprContext parseExpr() throws RecognitionException {
		ParseExprContext _localctx = new ParseExprContext(_ctx, getState());
		enterRule(_localctx, 6, RULE_parseExpr);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(65); 
			_errHandler.sync(this);
			_la = _input.LA(1);
			do {
				{
				{
				setState(64); match(ID);
				}
				}
				setState(67); 
				_errHandler.sync(this);
				_la = _input.LA(1);
			} while ( _la==ID );
			setState(78);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==OR) {
				{
				{
				setState(69); match(OR);
				setState(73);
				_errHandler.sync(this);
				_la = _input.LA(1);
				while (_la==ID) {
					{
					{
					setState(70); match(ID);
					}
					}
					setState(75);
					_errHandler.sync(this);
					_la = _input.LA(1);
				}
				}
				}
				setState(80);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class LexerRuleContext extends ParserRuleContext {
		public LexerRule r;
		public Token LexerID;
		public TokenContext token;
		public TerminalNode COLON() { return getToken(GRMParser.COLON, 0); }
		public TerminalNode LexerID() { return getToken(GRMParser.LexerID, 0); }
		public TerminalNode SEMICOLON() { return getToken(GRMParser.SEMICOLON, 0); }
		public TokenContext token() {
			return getRuleContext(TokenContext.class,0);
		}
		public LexerRuleContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_lexerRule; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof GRMListener ) ((GRMListener)listener).enterLexerRule(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof GRMListener ) ((GRMListener)listener).exitLexerRule(this);
		}
	}

	public final LexerRuleContext lexerRule() throws RecognitionException {
		LexerRuleContext _localctx = new LexerRuleContext(_ctx, getState());
		enterRule(_localctx, 8, RULE_lexerRule);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(81); ((LexerRuleContext)_localctx).LexerID = match(LexerID);
			setState(82); match(COLON);
			setState(83); ((LexerRuleContext)_localctx).token = token();
			setState(84); match(SEMICOLON);

			        ((LexerRuleContext)_localctx).r =  new LexerRule((((LexerRuleContext)_localctx).LexerID!=null?((LexerRuleContext)_localctx).LexerID.getText():null), (((LexerRuleContext)_localctx).token!=null?_input.getText(((LexerRuleContext)_localctx).token.start,((LexerRuleContext)_localctx).token.stop):null).substring(1, (((LexerRuleContext)_localctx).token!=null?_input.getText(((LexerRuleContext)_localctx).token.start,((LexerRuleContext)_localctx).token.stop):null).length() - 1));
			    
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class TokenContext extends ParserRuleContext {
		public TerminalNode TOKEN() { return getToken(GRMParser.TOKEN, 0); }
		public TokenContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_token; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof GRMListener ) ((GRMListener)listener).enterToken(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof GRMListener ) ((GRMListener)listener).exitToken(this);
		}
	}

	public final TokenContext token() throws RecognitionException {
		TokenContext _localctx = new TokenContext(_ctx, getState());
		enterRule(_localctx, 10, RULE_token);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(87); match(TOKEN);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static final String _serializedATN =
		"\2\3\23\\\4\2\t\2\4\3\t\3\4\4\t\4\4\5\t\5\4\6\t\6\4\7\t\7\3\2\3\2\3\2"+
		"\3\2\3\2\6\2\24\n\2\r\2\16\2\25\3\2\3\2\3\2\3\2\6\2\34\n\2\r\2\16\2\35"+
		"\3\2\3\2\3\2\3\2\6\2$\n\2\r\2\16\2%\5\2(\n\2\3\2\3\2\3\3\3\3\3\3\3\3\3"+
		"\3\5\3\61\n\3\3\3\3\3\3\3\3\3\3\3\3\4\3\4\3\4\3\4\3\4\6\4=\n\4\r\4\16"+
		"\4>\3\4\3\4\3\5\6\5D\n\5\r\5\16\5E\3\5\3\5\7\5J\n\5\f\5\16\5M\13\5\7\5"+
		"O\n\5\f\5\16\5R\13\5\3\6\3\6\3\6\3\6\3\6\3\6\3\7\3\7\3\7\2\b\2\4\6\b\n"+
		"\f\2\2^\2\16\3\2\2\2\4+\3\2\2\2\6\67\3\2\2\2\bC\3\2\2\2\nS\3\2\2\2\fY"+
		"\3\2\2\2\16\17\b\2\1\2\17\20\7\6\2\2\20\21\b\2\1\2\21\23\7\3\2\2\22\24"+
		"\5\4\3\2\23\22\3\2\2\2\24\25\3\2\2\2\25\23\3\2\2\2\25\26\3\2\2\2\26\27"+
		"\3\2\2\2\27\33\7\4\2\2\30\31\5\n\6\2\31\32\b\2\1\2\32\34\3\2\2\2\33\30"+
		"\3\2\2\2\34\35\3\2\2\2\35\33\3\2\2\2\35\36\3\2\2\2\36\'\3\2\2\2\37#\7"+
		"\5\2\2 !\5\n\6\2!\"\b\2\1\2\"$\3\2\2\2# \3\2\2\2$%\3\2\2\2%#\3\2\2\2%"+
		"&\3\2\2\2&(\3\2\2\2\'\37\3\2\2\2\'(\3\2\2\2()\3\2\2\2)*\7\1\2\2*\3\3\2"+
		"\2\2+,\b\3\1\2,\60\7\7\2\2-.\5\6\4\2./\b\3\1\2/\61\3\2\2\2\60-\3\2\2\2"+
		"\60\61\3\2\2\2\61\62\3\2\2\2\62\63\7\16\2\2\63\64\5\b\5\2\64\65\7\17\2"+
		"\2\65\66\b\3\1\2\66\5\3\2\2\2\678\b\4\1\28<\7\f\2\29:\7\t\2\2:;\7\t\2"+
		"\2;=\b\4\1\2<9\3\2\2\2=>\3\2\2\2><\3\2\2\2>?\3\2\2\2?@\3\2\2\2@A\7\r\2"+
		"\2A\7\3\2\2\2BD\7\6\2\2CB\3\2\2\2DE\3\2\2\2EC\3\2\2\2EF\3\2\2\2FP\3\2"+
		"\2\2GK\7\20\2\2HJ\7\6\2\2IH\3\2\2\2JM\3\2\2\2KI\3\2\2\2KL\3\2\2\2LO\3"+
		"\2\2\2MK\3\2\2\2NG\3\2\2\2OR\3\2\2\2PN\3\2\2\2PQ\3\2\2\2Q\t\3\2\2\2RP"+
		"\3\2\2\2ST\7\b\2\2TU\7\16\2\2UV\5\f\7\2VW\7\17\2\2WX\b\6\1\2X\13\3\2\2"+
		"\2YZ\7\23\2\2Z\r\3\2\2\2\13\25\35%\'\60>EKP";
	public static final ATN _ATN =
		ATNSimulator.deserialize(_serializedATN.toCharArray());
	static {
		_decisionToDFA = new DFA[_ATN.getNumberOfDecisions()];
	}
}