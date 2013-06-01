// Generated from GRM.g4 by ANTLR 4.0

import java.util.*;
import java.io.*;

import org.antlr.v4.runtime.tree.*;
import org.antlr.v4.runtime.Token;

public interface GRMListener extends ParseTreeListener {
	void enterParseExpr(GRMParser.ParseExprContext ctx);
	void exitParseExpr(GRMParser.ParseExprContext ctx);

	void enterToken(GRMParser.TokenContext ctx);
	void exitToken(GRMParser.TokenContext ctx);

	void enterArgs(GRMParser.ArgsContext ctx);
	void exitArgs(GRMParser.ArgsContext ctx);

	void enterFile(GRMParser.FileContext ctx);
	void exitFile(GRMParser.FileContext ctx);

	void enterParsing(GRMParser.ParsingContext ctx);
	void exitParsing(GRMParser.ParsingContext ctx);

	void enterLexerRule(GRMParser.LexerRuleContext ctx);
	void exitLexerRule(GRMParser.LexerRuleContext ctx);
}