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
		START_PARSER=1, START_LEXER=2, START_SKIP=3, VARS_B=4, JVar=5, JCode=6, 
		ParserID=7, LexerID=8, ID=9, LCB=10, RCB=11, LB=12, RB=13, COLON=14, SEMICOLON=15, 
		OR=16, COMMENT=17, WS=18, TOKEN=19;
	public static final String[] tokenNames = {
		"<INVALID>", "'_PARSER'", "'_LEXER'", "'_SKIP'", "VARS_B", "JVar", "JCode", 
		"ParserID", "LexerID", "ID", "'{\n'", "'}\n'", "'[\n'", "']\n'", "':'", 
		"';'", "'|'", "COMMENT", "WS", "TOKEN"
	};
	public static final int
		RULE_file = 0, RULE_parsing = 1, RULE_args = 2, RULE_vars = 3, RULE_code = 4, 
		RULE_parseExpr = 5, RULE_parseOption = 6, RULE_itemID = 7, RULE_lexerRule = 8, 
		RULE_token = 9;
	public static final String[] ruleNames = {
		"file", "parsing", "args", "vars", "code", "parseExpr", "parseOption", 
		"itemID", "lexerRule", "token"
	};

	@Override
	public String getGrammarFileName() { return "GRM.g4"; }

	@Override
	public String[] getTokenNames() { return tokenNames; }

	@Override
	public String[] getRuleNames() { return ruleNames; }

	@Override
	public ATN getATN() { return _ATN; }


	    String crop(String x) {
	        return x.substring(1, x.length() - 1);
	    }

	public GRMParser(TokenStream input) {
		super(input);
		_interp = new ParserATNSimulator(this,_ATN,_decisionToDFA,_sharedContextCache);
	}
	public static class FileContext extends ParserRuleContext {
		public String name;
		public ArrayList<LexerRule> lexerRules;
		public ArrayList<LexerRule> skipRules;
		public ArrayList<ParserRule> parserRules;
		public Token LexerID;
		public ParsingContext parsing;
		public LexerRuleContext lexerRule;
		public TerminalNode LexerID() { return getToken(GRMParser.LexerID, 0); }
		public TerminalNode START_PARSER() { return getToken(GRMParser.START_PARSER, 0); }
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
		public ParsingContext parsing(int i) {
			return getRuleContext(ParsingContext.class,i);
		}
		public TerminalNode START_SKIP() { return getToken(GRMParser.START_SKIP, 0); }
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
			        ((FileContext)_localctx).parserRules =  new ArrayList<ParserRule>();
			    
			setState(21); ((FileContext)_localctx).LexerID = match(LexerID);
			 ((FileContext)_localctx).name =  (((FileContext)_localctx).LexerID!=null?((FileContext)_localctx).LexerID.getText():null); 
			setState(23); match(START_PARSER);
			setState(27); 
			_errHandler.sync(this);
			_la = _input.LA(1);
			do {
				{
				{
				setState(24); ((FileContext)_localctx).parsing = parsing();
				 _localctx.parserRules.add(((FileContext)_localctx).parsing.r); 
				}
				}
				setState(29); 
				_errHandler.sync(this);
				_la = _input.LA(1);
			} while ( _la==ParserID );
			setState(31); match(START_LEXER);
			setState(35); 
			_errHandler.sync(this);
			_la = _input.LA(1);
			do {
				{
				{
				setState(32); ((FileContext)_localctx).lexerRule = lexerRule();
				 _localctx.lexerRules.add(((FileContext)_localctx).lexerRule.r); 
				}
				}
				setState(37); 
				_errHandler.sync(this);
				_la = _input.LA(1);
			} while ( _la==LexerID );
			setState(39); match(START_SKIP);
			setState(43); 
			_errHandler.sync(this);
			_la = _input.LA(1);
			do {
				{
				{
				setState(40); ((FileContext)_localctx).lexerRule = lexerRule();
				 _localctx.skipRules.add(((FileContext)_localctx).lexerRule.r); 
				}
				}
				setState(45); 
				_errHandler.sync(this);
				_la = _input.LA(1);
			} while ( _la==LexerID );
			setState(47); match(EOF);
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
		public VarsContext vars;
		public CodeContext code;
		public ParseExprContext parseExpr;
		public TerminalNode COLON() { return getToken(GRMParser.COLON, 0); }
		public VarsContext vars() {
			return getRuleContext(VarsContext.class,0);
		}
		public ParseExprContext parseExpr() {
			return getRuleContext(ParseExprContext.class,0);
		}
		public TerminalNode ParserID() { return getToken(GRMParser.ParserID, 0); }
		public TerminalNode SEMICOLON() { return getToken(GRMParser.SEMICOLON, 0); }
		public ArgsContext args() {
			return getRuleContext(ArgsContext.class,0);
		}
		public CodeContext code() {
			return getRuleContext(CodeContext.class,0);
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

			        ArrayList<String> args = new ArrayList<String>();
			        ArrayList<String> vars = new ArrayList<String>();
			        String ic = "";
			    
			setState(50); ((ParsingContext)_localctx).ParserID = match(ParserID);
			setState(54);
			_la = _input.LA(1);
			if (_la==LB) {
				{
				setState(51); ((ParsingContext)_localctx).args = args();
				 args = ((ParsingContext)_localctx).args.r; 
				}
			}

			setState(59);
			_la = _input.LA(1);
			if (_la==VARS_B) {
				{
				setState(56); ((ParsingContext)_localctx).vars = vars();
				 vars = ((ParsingContext)_localctx).vars.r; 
				}
			}

			setState(61); match(COLON);
			setState(65);
			_la = _input.LA(1);
			if (_la==LCB) {
				{
				setState(62); ((ParsingContext)_localctx).code = code();
				 ic = ((ParsingContext)_localctx).code.r; 
				}
			}

			setState(67); ((ParsingContext)_localctx).parseExpr = parseExpr();
			setState(68); match(SEMICOLON);
			 ((ParsingContext)_localctx).r =  new ParserRule((((ParsingContext)_localctx).ParserID!=null?((ParsingContext)_localctx).ParserID.getText():null), args, vars, ic, ((ParsingContext)_localctx).parseExpr.r); 
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
		public ArrayList<String> r;
		public Token JVar;
		public TerminalNode LB() { return getToken(GRMParser.LB, 0); }
		public TerminalNode JVar(int i) {
			return getToken(GRMParser.JVar, i);
		}
		public List<TerminalNode> JVar() { return getTokens(GRMParser.JVar); }
		public TerminalNode RB() { return getToken(GRMParser.RB, 0); }
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
			setState(71); match(LB);
			 ((ArgsContext)_localctx).r =  new ArrayList<String>(); 
			setState(75); 
			_errHandler.sync(this);
			_la = _input.LA(1);
			do {
				{
				{
				setState(73); ((ArgsContext)_localctx).JVar = match(JVar);
				 _localctx.r.add(crop((((ArgsContext)_localctx).JVar!=null?((ArgsContext)_localctx).JVar.getText():null))); 
				}
				}
				setState(77); 
				_errHandler.sync(this);
				_la = _input.LA(1);
			} while ( _la==JVar );
			setState(79); match(RB);
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

	public static class VarsContext extends ParserRuleContext {
		public ArrayList<String> r;
		public Token JVar;
		public TerminalNode JVar(int i) {
			return getToken(GRMParser.JVar, i);
		}
		public List<TerminalNode> JVar() { return getTokens(GRMParser.JVar); }
		public TerminalNode RB() { return getToken(GRMParser.RB, 0); }
		public TerminalNode VARS_B() { return getToken(GRMParser.VARS_B, 0); }
		public VarsContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_vars; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof GRMListener ) ((GRMListener)listener).enterVars(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof GRMListener ) ((GRMListener)listener).exitVars(this);
		}
	}

	public final VarsContext vars() throws RecognitionException {
		VarsContext _localctx = new VarsContext(_ctx, getState());
		enterRule(_localctx, 6, RULE_vars);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(81); match(VARS_B);
			 ((VarsContext)_localctx).r =  new ArrayList<String>(); 
			setState(85); 
			_errHandler.sync(this);
			_la = _input.LA(1);
			do {
				{
				{
				setState(83); ((VarsContext)_localctx).JVar = match(JVar);
				 _localctx.r.add(crop((((VarsContext)_localctx).JVar!=null?((VarsContext)_localctx).JVar.getText():null))); 
				}
				}
				setState(87); 
				_errHandler.sync(this);
				_la = _input.LA(1);
			} while ( _la==JVar );
			setState(89); match(RB);
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

	public static class CodeContext extends ParserRuleContext {
		public String r;;
		public Token JCode;
		public TerminalNode JCode(int i) {
			return getToken(GRMParser.JCode, i);
		}
		public List<TerminalNode> JCode() { return getTokens(GRMParser.JCode); }
		public TerminalNode RCB() { return getToken(GRMParser.RCB, 0); }
		public TerminalNode LCB() { return getToken(GRMParser.LCB, 0); }
		public CodeContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_code; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof GRMListener ) ((GRMListener)listener).enterCode(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof GRMListener ) ((GRMListener)listener).exitCode(this);
		}
	}

	public final CodeContext code() throws RecognitionException {
		CodeContext _localctx = new CodeContext(_ctx, getState());
		enterRule(_localctx, 8, RULE_code);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(91); match(LCB);
			 ArrayList<String> a = new ArrayList<String>(); 
			setState(95); 
			_errHandler.sync(this);
			_la = _input.LA(1);
			do {
				{
				{
				setState(93); ((CodeContext)_localctx).JCode = match(JCode);
				 a.add((((CodeContext)_localctx).JCode!=null?((CodeContext)_localctx).JCode.getText():null).substring(1)); 
				}
				}
				setState(97); 
				_errHandler.sync(this);
				_la = _input.LA(1);
			} while ( _la==JCode );

			        StringBuilder sb = new StringBuilder();
			        for (String s : a) {
			            sb.append(s);
			            sb.append('\n');
			        }
			        ((CodeContext)_localctx).r =  sb.toString();
			    
			setState(100); match(RCB);
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
		public ArrayList<ArrayList<ParseItem>> r;
		public ParseOptionContext parseOption;
		public List<ParseOptionContext> parseOption() {
			return getRuleContexts(ParseOptionContext.class);
		}
		public TerminalNode OR(int i) {
			return getToken(GRMParser.OR, i);
		}
		public ParseOptionContext parseOption(int i) {
			return getRuleContext(ParseOptionContext.class,i);
		}
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
		enterRule(_localctx, 10, RULE_parseExpr);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			 ((ParseExprContext)_localctx).r =  new ArrayList<ArrayList<ParseItem>>(); 
			setState(103); ((ParseExprContext)_localctx).parseOption = parseOption();
			 _localctx.r.add(((ParseExprContext)_localctx).parseOption.r); 
			setState(111);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==OR) {
				{
				{
				setState(105); match(OR);
				setState(106); ((ParseExprContext)_localctx).parseOption = parseOption();
				 _localctx.r.add(((ParseExprContext)_localctx).parseOption.r); 
				}
				}
				setState(113);
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

	public static class ParseOptionContext extends ParserRuleContext {
		public ArrayList<ParseItem> r;
		public ItemIDContext itemID;
		public CodeContext code;
		public List<ItemIDContext> itemID() {
			return getRuleContexts(ItemIDContext.class);
		}
		public CodeContext code(int i) {
			return getRuleContext(CodeContext.class,i);
		}
		public List<CodeContext> code() {
			return getRuleContexts(CodeContext.class);
		}
		public ItemIDContext itemID(int i) {
			return getRuleContext(ItemIDContext.class,i);
		}
		public ParseOptionContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_parseOption; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof GRMListener ) ((GRMListener)listener).enterParseOption(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof GRMListener ) ((GRMListener)listener).exitParseOption(this);
		}
	}

	public final ParseOptionContext parseOption() throws RecognitionException {
		ParseOptionContext _localctx = new ParseOptionContext(_ctx, getState());
		enterRule(_localctx, 12, RULE_parseOption);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			 ((ParseOptionContext)_localctx).r =  new ArrayList<ParseItem>(); 
			setState(124); 
			_errHandler.sync(this);
			_la = _input.LA(1);
			do {
				{
				{
				 String c = ""; 
				setState(116); ((ParseOptionContext)_localctx).itemID = itemID();
				setState(120);
				_la = _input.LA(1);
				if (_la==LCB) {
					{
					setState(117); ((ParseOptionContext)_localctx).code = code();
					 c = ((ParseOptionContext)_localctx).code.r; 
					}
				}

				 _localctx.r.add(new ParseItem((((ParseOptionContext)_localctx).itemID!=null?_input.getText(((ParseOptionContext)_localctx).itemID.start,((ParseOptionContext)_localctx).itemID.stop):null), c)); 
				}
				}
				setState(126); 
				_errHandler.sync(this);
				_la = _input.LA(1);
			} while ( _la==ParserID || _la==LexerID );
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

	public static class ItemIDContext extends ParserRuleContext {
		public TerminalNode LexerID() { return getToken(GRMParser.LexerID, 0); }
		public TerminalNode ParserID() { return getToken(GRMParser.ParserID, 0); }
		public ItemIDContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_itemID; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof GRMListener ) ((GRMListener)listener).enterItemID(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof GRMListener ) ((GRMListener)listener).exitItemID(this);
		}
	}

	public final ItemIDContext itemID() throws RecognitionException {
		ItemIDContext _localctx = new ItemIDContext(_ctx, getState());
		enterRule(_localctx, 14, RULE_itemID);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(128);
			_la = _input.LA(1);
			if ( !(_la==ParserID || _la==LexerID) ) {
			_errHandler.recoverInline(this);
			}
			consume();
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
		enterRule(_localctx, 16, RULE_lexerRule);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(130); ((LexerRuleContext)_localctx).LexerID = match(LexerID);
			setState(131); match(COLON);
			setState(132); ((LexerRuleContext)_localctx).token = token();
			setState(133); match(SEMICOLON);
			 ((LexerRuleContext)_localctx).r =  new LexerRule((((LexerRuleContext)_localctx).LexerID!=null?((LexerRuleContext)_localctx).LexerID.getText():null), ((LexerRuleContext)_localctx).token.r); 
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
		public String r;
		public Token TOKEN;
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
		enterRule(_localctx, 18, RULE_token);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(136); ((TokenContext)_localctx).TOKEN = match(TOKEN);
			 ((TokenContext)_localctx).r =  crop((((TokenContext)_localctx).TOKEN!=null?((TokenContext)_localctx).TOKEN.getText():null)); 
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
		"\2\3\25\u008e\4\2\t\2\4\3\t\3\4\4\t\4\4\5\t\5\4\6\t\6\4\7\t\7\4\b\t\b"+
		"\4\t\t\t\4\n\t\n\4\13\t\13\3\2\3\2\3\2\3\2\3\2\3\2\3\2\6\2\36\n\2\r\2"+
		"\16\2\37\3\2\3\2\3\2\3\2\6\2&\n\2\r\2\16\2\'\3\2\3\2\3\2\3\2\6\2.\n\2"+
		"\r\2\16\2/\3\2\3\2\3\3\3\3\3\3\3\3\3\3\5\39\n\3\3\3\3\3\3\3\5\3>\n\3\3"+
		"\3\3\3\3\3\3\3\5\3D\n\3\3\3\3\3\3\3\3\3\3\4\3\4\3\4\3\4\6\4N\n\4\r\4\16"+
		"\4O\3\4\3\4\3\5\3\5\3\5\3\5\6\5X\n\5\r\5\16\5Y\3\5\3\5\3\6\3\6\3\6\3\6"+
		"\6\6b\n\6\r\6\16\6c\3\6\3\6\3\6\3\7\3\7\3\7\3\7\3\7\3\7\3\7\7\7p\n\7\f"+
		"\7\16\7s\13\7\3\b\3\b\3\b\3\b\3\b\3\b\5\b{\n\b\3\b\3\b\6\b\177\n\b\r\b"+
		"\16\b\u0080\3\t\3\t\3\n\3\n\3\n\3\n\3\n\3\n\3\13\3\13\3\13\3\13\2\f\2"+
		"\4\6\b\n\f\16\20\22\24\2\3\3\t\n\u008f\2\26\3\2\2\2\4\63\3\2\2\2\6I\3"+
		"\2\2\2\bS\3\2\2\2\n]\3\2\2\2\fh\3\2\2\2\16t\3\2\2\2\20\u0082\3\2\2\2\22"+
		"\u0084\3\2\2\2\24\u008a\3\2\2\2\26\27\b\2\1\2\27\30\7\n\2\2\30\31\b\2"+
		"\1\2\31\35\7\3\2\2\32\33\5\4\3\2\33\34\b\2\1\2\34\36\3\2\2\2\35\32\3\2"+
		"\2\2\36\37\3\2\2\2\37\35\3\2\2\2\37 \3\2\2\2 !\3\2\2\2!%\7\4\2\2\"#\5"+
		"\22\n\2#$\b\2\1\2$&\3\2\2\2%\"\3\2\2\2&\'\3\2\2\2\'%\3\2\2\2\'(\3\2\2"+
		"\2()\3\2\2\2)-\7\5\2\2*+\5\22\n\2+,\b\2\1\2,.\3\2\2\2-*\3\2\2\2./\3\2"+
		"\2\2/-\3\2\2\2/\60\3\2\2\2\60\61\3\2\2\2\61\62\7\1\2\2\62\3\3\2\2\2\63"+
		"\64\b\3\1\2\648\7\t\2\2\65\66\5\6\4\2\66\67\b\3\1\2\679\3\2\2\28\65\3"+
		"\2\2\289\3\2\2\29=\3\2\2\2:;\5\b\5\2;<\b\3\1\2<>\3\2\2\2=:\3\2\2\2=>\3"+
		"\2\2\2>?\3\2\2\2?C\7\20\2\2@A\5\n\6\2AB\b\3\1\2BD\3\2\2\2C@\3\2\2\2CD"+
		"\3\2\2\2DE\3\2\2\2EF\5\f\7\2FG\7\21\2\2GH\b\3\1\2H\5\3\2\2\2IJ\7\16\2"+
		"\2JM\b\4\1\2KL\7\7\2\2LN\b\4\1\2MK\3\2\2\2NO\3\2\2\2OM\3\2\2\2OP\3\2\2"+
		"\2PQ\3\2\2\2QR\7\17\2\2R\7\3\2\2\2ST\7\6\2\2TW\b\5\1\2UV\7\7\2\2VX\b\5"+
		"\1\2WU\3\2\2\2XY\3\2\2\2YW\3\2\2\2YZ\3\2\2\2Z[\3\2\2\2[\\\7\17\2\2\\\t"+
		"\3\2\2\2]^\7\f\2\2^a\b\6\1\2_`\7\b\2\2`b\b\6\1\2a_\3\2\2\2bc\3\2\2\2c"+
		"a\3\2\2\2cd\3\2\2\2de\3\2\2\2ef\b\6\1\2fg\7\r\2\2g\13\3\2\2\2hi\b\7\1"+
		"\2ij\5\16\b\2jq\b\7\1\2kl\7\22\2\2lm\5\16\b\2mn\b\7\1\2np\3\2\2\2ok\3"+
		"\2\2\2ps\3\2\2\2qo\3\2\2\2qr\3\2\2\2r\r\3\2\2\2sq\3\2\2\2t~\b\b\1\2uv"+
		"\b\b\1\2vz\5\20\t\2wx\5\n\6\2xy\b\b\1\2y{\3\2\2\2zw\3\2\2\2z{\3\2\2\2"+
		"{|\3\2\2\2|}\b\b\1\2}\177\3\2\2\2~u\3\2\2\2\177\u0080\3\2\2\2\u0080~\3"+
		"\2\2\2\u0080\u0081\3\2\2\2\u0081\17\3\2\2\2\u0082\u0083\t\2\2\2\u0083"+
		"\21\3\2\2\2\u0084\u0085\7\n\2\2\u0085\u0086\7\20\2\2\u0086\u0087\5\24"+
		"\13\2\u0087\u0088\7\21\2\2\u0088\u0089\b\n\1\2\u0089\23\3\2\2\2\u008a"+
		"\u008b\7\25\2\2\u008b\u008c\b\13\1\2\u008c\25\3\2\2\2\16\37\'/8=COYcq"+
		"z\u0080";
	public static final ATN _ATN =
		ATNSimulator.deserialize(_serializedATN.toCharArray());
	static {
		_decisionToDFA = new DFA[_ATN.getNumberOfDecisions()];
	}
}