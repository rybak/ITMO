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
		START_IMPORT=1, START_PARSER=2, START_LEXER=3, START_SKIP=4, ArgVar=5, 
		RetVar=6, JCode=7, CallArgs=8, ParserID=9, LexerID=10, ID=11, COLON=12, 
		SEMICOLON=13, OR=14, COMMENT=15, WS=16, TOKEN=17;
	public static final String[] tokenNames = {
		"<INVALID>", "'_IMPORT'", "'_PARSER'", "'_LEXER'", "'_SKIP'", "ArgVar", 
		"RetVar", "JCode", "CallArgs", "ParserID", "LexerID", "ID", "':'", "';'", 
		"'|'", "COMMENT", "WS", "TOKEN"
	};
	public static final int
		RULE_file = 0, RULE_parsing = 1, RULE_args = 2, RULE_vars = 3, RULE_code = 4, 
		RULE_parseExpr = 5, RULE_parseOption = 6, RULE_itemID = 7, RULE_callArgs = 8, 
		RULE_lexerRule = 9, RULE_token = 10;
	public static final String[] ruleNames = {
		"file", "parsing", "args", "vars", "code", "parseExpr", "parseOption", 
		"itemID", "callArgs", "lexerRule", "token"
	};

	@Override
	public String getGrammarFileName() { return "GRM.g4"; }

	@Override
	public String[] getTokenNames() { return tokenNames; }

	@Override
	public String[] getRuleNames() { return ruleNames; }

	@Override
	public ATN getATN() { return _ATN; }


	    String crop(String x, int n) {
	        return x.substring(n, x.length() - n);
	    }

	public GRMParser(TokenStream input) {
		super(input);
		_interp = new ParserATNSimulator(this,_ATN,_decisionToDFA,_sharedContextCache);
	}
	public static class FileContext extends ParserRuleContext {
		public String name;
		public String imports;
		public ArrayList<LexerRule> lexerRules;
		public ArrayList<LexerRule> skipRules;
		public ArrayList<ParserRule> parserRules;
		public Token LexerID;
		public CodeContext code;
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
		public CodeContext code() {
			return getRuleContext(CodeContext.class,0);
		}
		public TerminalNode START_LEXER() { return getToken(GRMParser.START_LEXER, 0); }
		public LexerRuleContext lexerRule(int i) {
			return getRuleContext(LexerRuleContext.class,i);
		}
		public ParsingContext parsing(int i) {
			return getRuleContext(ParsingContext.class,i);
		}
		public TerminalNode START_SKIP() { return getToken(GRMParser.START_SKIP, 0); }
		public TerminalNode START_IMPORT() { return getToken(GRMParser.START_IMPORT, 0); }
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
			        ((FileContext)_localctx).start =  null;
			    
			setState(23); ((FileContext)_localctx).LexerID = match(LexerID);
			 ((FileContext)_localctx).name =  (((FileContext)_localctx).LexerID!=null?((FileContext)_localctx).LexerID.getText():null); 
			setState(25); match(START_IMPORT);
			setState(26); ((FileContext)_localctx).code = code();
			 ((FileContext)_localctx).imports =  ((FileContext)_localctx).code.r; 
			setState(28); match(START_PARSER);
			setState(32); 
			_errHandler.sync(this);
			_la = _input.LA(1);
			do {
				{
				{
				setState(29); ((FileContext)_localctx).parsing = parsing();

				        _localctx.parserRules.add(((FileContext)_localctx).parsing.r);
				    
				}
				}
				setState(34); 
				_errHandler.sync(this);
				_la = _input.LA(1);
			} while ( _la==ParserID );
			setState(36); match(START_LEXER);
			setState(40); 
			_errHandler.sync(this);
			_la = _input.LA(1);
			do {
				{
				{
				setState(37); ((FileContext)_localctx).lexerRule = lexerRule();
				 _localctx.lexerRules.add(((FileContext)_localctx).lexerRule.r); 
				}
				}
				setState(42); 
				_errHandler.sync(this);
				_la = _input.LA(1);
			} while ( _la==LexerID );
			setState(44); match(START_SKIP);
			setState(48); 
			_errHandler.sync(this);
			_la = _input.LA(1);
			do {
				{
				{
				setState(45); ((FileContext)_localctx).lexerRule = lexerRule();
				 _localctx.skipRules.add(((FileContext)_localctx).lexerRule.r); 
				}
				}
				setState(50); 
				_errHandler.sync(this);
				_la = _input.LA(1);
			} while ( _la==LexerID );
			setState(52); match(EOF);
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
			    
			setState(55); ((ParsingContext)_localctx).ParserID = match(ParserID);
			setState(59);
			switch ( getInterpreter().adaptivePredict(_input,3,_ctx) ) {
			case 1:
				{
				setState(56); ((ParsingContext)_localctx).args = args();
				 args = ((ParsingContext)_localctx).args.r; 
				}
				break;
			}
			setState(64);
			switch ( getInterpreter().adaptivePredict(_input,4,_ctx) ) {
			case 1:
				{
				setState(61); ((ParsingContext)_localctx).vars = vars();
				 vars = ((ParsingContext)_localctx).vars.r; 
				}
				break;
			}
			setState(66); match(COLON);
			setState(70);
			_la = _input.LA(1);
			if (_la==JCode) {
				{
				setState(67); ((ParsingContext)_localctx).code = code();
				 ic = ((ParsingContext)_localctx).code.r; 
				}
			}

			setState(72); ((ParsingContext)_localctx).parseExpr = parseExpr();
			setState(73); match(SEMICOLON);
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
		public Token ArgVar;
		public TerminalNode ArgVar(int i) {
			return getToken(GRMParser.ArgVar, i);
		}
		public List<TerminalNode> ArgVar() { return getTokens(GRMParser.ArgVar); }
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
			 ((ArgsContext)_localctx).r =  new ArrayList<String>(); 
			setState(81);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==ArgVar) {
				{
				{
				setState(77); ((ArgsContext)_localctx).ArgVar = match(ArgVar);
				 _localctx.r.add(crop((((ArgsContext)_localctx).ArgVar!=null?((ArgsContext)_localctx).ArgVar.getText():null), 1)); 
				}
				}
				setState(83);
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

	public static class VarsContext extends ParserRuleContext {
		public ArrayList<String> r;
		public Token RetVar;
		public TerminalNode RetVar(int i) {
			return getToken(GRMParser.RetVar, i);
		}
		public List<TerminalNode> RetVar() { return getTokens(GRMParser.RetVar); }
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
			 ((VarsContext)_localctx).r =  new ArrayList<String>(); 
			setState(89);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==RetVar) {
				{
				{
				setState(85); ((VarsContext)_localctx).RetVar = match(RetVar);
				 _localctx.r.add(crop((((VarsContext)_localctx).RetVar!=null?((VarsContext)_localctx).RetVar.getText():null), 1)); 
				}
				}
				setState(91);
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

	public static class CodeContext extends ParserRuleContext {
		public String r;;
		public Token JCode;
		public TerminalNode JCode() { return getToken(GRMParser.JCode, 0); }
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
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(92); ((CodeContext)_localctx).JCode = match(JCode);

			        ((CodeContext)_localctx).r =  crop((((CodeContext)_localctx).JCode!=null?((CodeContext)_localctx).JCode.getText():null), 2);
			    
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
			setState(96); ((ParseExprContext)_localctx).parseOption = parseOption();
			 _localctx.r.add(((ParseExprContext)_localctx).parseOption.r); 
			setState(104);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==OR) {
				{
				{
				setState(98); match(OR);
				setState(99); ((ParseExprContext)_localctx).parseOption = parseOption();
				 _localctx.r.add(((ParseExprContext)_localctx).parseOption.r); 
				}
				}
				setState(106);
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
			setState(117); 
			_errHandler.sync(this);
			_la = _input.LA(1);
			do {
				{
				{
				 String c = ""; 
				setState(109); ((ParseOptionContext)_localctx).itemID = itemID();
				setState(113);
				_la = _input.LA(1);
				if (_la==JCode) {
					{
					setState(110); ((ParseOptionContext)_localctx).code = code();
					 c = ((ParseOptionContext)_localctx).code.r; 
					}
				}


				            _localctx.r.add(new ParseItem(((ParseOptionContext)_localctx).itemID.id, c, ((ParseOptionContext)_localctx).itemID.call));
				        
				}
				}
				setState(119); 
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
		public String id;
		public String call;
		public Token ParserID;
		public CallArgsContext callArgs;
		public Token LexerID;
		public TerminalNode LexerID() { return getToken(GRMParser.LexerID, 0); }
		public CallArgsContext callArgs() {
			return getRuleContext(CallArgsContext.class,0);
		}
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
			setState(130);
			switch (_input.LA(1)) {
			case ParserID:
				enterOuterAlt(_localctx, 1);
				{
				{
				setState(121); ((ItemIDContext)_localctx).ParserID = match(ParserID);

				            ((ItemIDContext)_localctx).call =  "()";
				            ((ItemIDContext)_localctx).id =  (((ItemIDContext)_localctx).ParserID!=null?((ItemIDContext)_localctx).ParserID.getText():null);
				        
				setState(126);
				_la = _input.LA(1);
				if (_la==CallArgs) {
					{
					setState(123); ((ItemIDContext)_localctx).callArgs = callArgs();
					 ((ItemIDContext)_localctx).call =  crop((((ItemIDContext)_localctx).callArgs!=null?_input.getText(((ItemIDContext)_localctx).callArgs.start,((ItemIDContext)_localctx).callArgs.stop):null), 1); 
					}
				}

				}
				}
				break;
			case LexerID:
				enterOuterAlt(_localctx, 2);
				{
				{
				setState(128); ((ItemIDContext)_localctx).LexerID = match(LexerID);

				            ((ItemIDContext)_localctx).id =  (((ItemIDContext)_localctx).LexerID!=null?((ItemIDContext)_localctx).LexerID.getText():null);
				            ((ItemIDContext)_localctx).call =  null;
				        
				}
				}
				break;
			default:
				throw new NoViableAltException(this);
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

	public static class CallArgsContext extends ParserRuleContext {
		public TerminalNode CallArgs() { return getToken(GRMParser.CallArgs, 0); }
		public CallArgsContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_callArgs; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof GRMListener ) ((GRMListener)listener).enterCallArgs(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof GRMListener ) ((GRMListener)listener).exitCallArgs(this);
		}
	}

	public final CallArgsContext callArgs() throws RecognitionException {
		CallArgsContext _localctx = new CallArgsContext(_ctx, getState());
		enterRule(_localctx, 16, RULE_callArgs);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(132); match(CallArgs);
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
		enterRule(_localctx, 18, RULE_lexerRule);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(134); ((LexerRuleContext)_localctx).LexerID = match(LexerID);
			setState(135); match(COLON);
			setState(136); ((LexerRuleContext)_localctx).token = token();
			setState(137); match(SEMICOLON);
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
		enterRule(_localctx, 20, RULE_token);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(140); ((TokenContext)_localctx).TOKEN = match(TOKEN);
			 ((TokenContext)_localctx).r =  crop((((TokenContext)_localctx).TOKEN!=null?((TokenContext)_localctx).TOKEN.getText():null), 1); 
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
		"\2\3\23\u0092\4\2\t\2\4\3\t\3\4\4\t\4\4\5\t\5\4\6\t\6\4\7\t\7\4\b\t\b"+
		"\4\t\t\t\4\n\t\n\4\13\t\13\4\f\t\f\3\2\3\2\3\2\3\2\3\2\3\2\3\2\3\2\3\2"+
		"\3\2\6\2#\n\2\r\2\16\2$\3\2\3\2\3\2\3\2\6\2+\n\2\r\2\16\2,\3\2\3\2\3\2"+
		"\3\2\6\2\63\n\2\r\2\16\2\64\3\2\3\2\3\3\3\3\3\3\3\3\3\3\5\3>\n\3\3\3\3"+
		"\3\3\3\5\3C\n\3\3\3\3\3\3\3\3\3\5\3I\n\3\3\3\3\3\3\3\3\3\3\4\3\4\3\4\7"+
		"\4R\n\4\f\4\16\4U\13\4\3\5\3\5\3\5\7\5Z\n\5\f\5\16\5]\13\5\3\6\3\6\3\6"+
		"\3\7\3\7\3\7\3\7\3\7\3\7\3\7\7\7i\n\7\f\7\16\7l\13\7\3\b\3\b\3\b\3\b\3"+
		"\b\3\b\5\bt\n\b\3\b\3\b\6\bx\n\b\r\b\16\by\3\t\3\t\3\t\3\t\3\t\5\t\u0081"+
		"\n\t\3\t\3\t\5\t\u0085\n\t\3\n\3\n\3\13\3\13\3\13\3\13\3\13\3\13\3\f\3"+
		"\f\3\f\3\f\2\r\2\4\6\b\n\f\16\20\22\24\26\2\2\u0093\2\30\3\2\2\2\48\3"+
		"\2\2\2\6N\3\2\2\2\bV\3\2\2\2\n^\3\2\2\2\fa\3\2\2\2\16m\3\2\2\2\20\u0084"+
		"\3\2\2\2\22\u0086\3\2\2\2\24\u0088\3\2\2\2\26\u008e\3\2\2\2\30\31\b\2"+
		"\1\2\31\32\7\f\2\2\32\33\b\2\1\2\33\34\7\3\2\2\34\35\5\n\6\2\35\36\b\2"+
		"\1\2\36\"\7\4\2\2\37 \5\4\3\2 !\b\2\1\2!#\3\2\2\2\"\37\3\2\2\2#$\3\2\2"+
		"\2$\"\3\2\2\2$%\3\2\2\2%&\3\2\2\2&*\7\5\2\2\'(\5\24\13\2()\b\2\1\2)+\3"+
		"\2\2\2*\'\3\2\2\2+,\3\2\2\2,*\3\2\2\2,-\3\2\2\2-.\3\2\2\2.\62\7\6\2\2"+
		"/\60\5\24\13\2\60\61\b\2\1\2\61\63\3\2\2\2\62/\3\2\2\2\63\64\3\2\2\2\64"+
		"\62\3\2\2\2\64\65\3\2\2\2\65\66\3\2\2\2\66\67\7\1\2\2\67\3\3\2\2\289\b"+
		"\3\1\29=\7\13\2\2:;\5\6\4\2;<\b\3\1\2<>\3\2\2\2=:\3\2\2\2=>\3\2\2\2>B"+
		"\3\2\2\2?@\5\b\5\2@A\b\3\1\2AC\3\2\2\2B?\3\2\2\2BC\3\2\2\2CD\3\2\2\2D"+
		"H\7\16\2\2EF\5\n\6\2FG\b\3\1\2GI\3\2\2\2HE\3\2\2\2HI\3\2\2\2IJ\3\2\2\2"+
		"JK\5\f\7\2KL\7\17\2\2LM\b\3\1\2M\5\3\2\2\2NS\b\4\1\2OP\7\7\2\2PR\b\4\1"+
		"\2QO\3\2\2\2RU\3\2\2\2SQ\3\2\2\2ST\3\2\2\2T\7\3\2\2\2US\3\2\2\2V[\b\5"+
		"\1\2WX\7\b\2\2XZ\b\5\1\2YW\3\2\2\2Z]\3\2\2\2[Y\3\2\2\2[\\\3\2\2\2\\\t"+
		"\3\2\2\2][\3\2\2\2^_\7\t\2\2_`\b\6\1\2`\13\3\2\2\2ab\b\7\1\2bc\5\16\b"+
		"\2cj\b\7\1\2de\7\20\2\2ef\5\16\b\2fg\b\7\1\2gi\3\2\2\2hd\3\2\2\2il\3\2"+
		"\2\2jh\3\2\2\2jk\3\2\2\2k\r\3\2\2\2lj\3\2\2\2mw\b\b\1\2no\b\b\1\2os\5"+
		"\20\t\2pq\5\n\6\2qr\b\b\1\2rt\3\2\2\2sp\3\2\2\2st\3\2\2\2tu\3\2\2\2uv"+
		"\b\b\1\2vx\3\2\2\2wn\3\2\2\2xy\3\2\2\2yw\3\2\2\2yz\3\2\2\2z\17\3\2\2\2"+
		"{|\7\13\2\2|\u0080\b\t\1\2}~\5\22\n\2~\177\b\t\1\2\177\u0081\3\2\2\2\u0080"+
		"}\3\2\2\2\u0080\u0081\3\2\2\2\u0081\u0085\3\2\2\2\u0082\u0083\7\f\2\2"+
		"\u0083\u0085\b\t\1\2\u0084{\3\2\2\2\u0084\u0082\3\2\2\2\u0085\21\3\2\2"+
		"\2\u0086\u0087\7\n\2\2\u0087\23\3\2\2\2\u0088\u0089\7\f\2\2\u0089\u008a"+
		"\7\16\2\2\u008a\u008b\5\26\f\2\u008b\u008c\7\17\2\2\u008c\u008d\b\13\1"+
		"\2\u008d\25\3\2\2\2\u008e\u008f\7\23\2\2\u008f\u0090\b\f\1\2\u0090\27"+
		"\3\2\2\2\17$,\64=BHS[jsy\u0080\u0084";
	public static final ATN _ATN =
		ATNSimulator.deserialize(_serializedATN.toCharArray());
	static {
		_decisionToDFA = new DFA[_ATN.getNumberOfDecisions()];
	}
}