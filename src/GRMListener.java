// Generated from GRM.g4 by ANTLR 4.0

import java.util.*;
import java.io.*;

import org.antlr.v4.runtime.tree.*;
import org.antlr.v4.runtime.Token;

public interface GRMListener extends ParseTreeListener {
	void enterParseOption(GRMParser.ParseOptionContext ctx);
	void exitParseOption(GRMParser.ParseOptionContext ctx);

	void enterVars(GRMParser.VarsContext ctx);
	void exitVars(GRMParser.VarsContext ctx);

	void enterParseExpr(GRMParser.ParseExprContext ctx);
	void exitParseExpr(GRMParser.ParseExprContext ctx);

	void enterItemID(GRMParser.ItemIDContext ctx);
	void exitItemID(GRMParser.ItemIDContext ctx);

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

	void enterCode(GRMParser.CodeContext ctx);
	void exitCode(GRMParser.CodeContext ctx);
}