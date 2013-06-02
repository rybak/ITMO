
import java.io.PrintWriter;
import java.util.*;

public class ParserGenerator {

	private PrintWriter out;
	private String name;
	private String LEX;
	private static final String EPS = "EPS";
	private static final String EOF = "EOF";
	private static final String PAR_THROWS_CB = ") throws ParseException {";
	private static final String EPS_ERROR = "EPS can be only in singletone rule";

	public ParserGenerator(PrintWriter out, String name) {
		this.out = out;
		this.name = name;
		LEX = name + Core.L;
	}

	public void generate(String imports, ArrayList<ParserRule> parserRules,
			Set<String> tokens) {
		tokens.add(EPS);
		printImports(imports);
		out.println(Core.buildClassHeader(name + Core.P, Core.P));
		analyzeRules(parserRules, tokens);
		printParseMethod(parserRules.get(0));
		printRules(parserRules);
		Core.close(out, 0);
		out.close();
	}

	private void printRules(ArrayList<ParserRule> parserRules) {
		for (ParserRule r : parserRules) {
			printRuleClass(r);
			printRuleMethod(r);
		}
	}

	private Map<String, Set<String>> FIRST = new HashMap<String, Set<String>>();

	private void analyzeRules(ArrayList<ParserRule> parserRules,
			Set<String> tokens) {
		initCalc(parserRules, tokens);
		calcFirst(parserRules);
	}

	private void calcFirst(ArrayList<ParserRule> parserRules) {
		boolean changes = true;
		while (changes) {
			changes = false;
			for (ParserRule rule : parserRules) {
				int oldSize = FIRST.get(rule.name).size();
				FIRST.get(rule.name).addAll(calcFirstForRule(rule));
				int newSize = FIRST.get(rule.name).size();
				if (newSize != oldSize) {
					changes = true;
				}
			}
		}
		if (parserRules.get(0).isEPS) {
			FIRST.get(parserRules.get(0).name).add(EOF);
		}
	}

	private Set<String> calcFirstForRule(ParserRule rule) {
		Set<String> a = new HashSet<String>();
		for (ArrayList<ParseItem> opt : rule.options) {
			a.addAll(calcFirstForOption(opt, 0));
		}
		if (a.contains(EPS)) {
			rule.isEPS = true;
		}
		return a;
	}

	private Set<String> calcFirstForOption(ArrayList<ParseItem> opt, int pos) {
		Set<String> a = new HashSet<String>();
		if (pos == opt.size()) {
			a.add(EPS);
		} else {
			String itemName = opt.get(pos).name;
			boolean isEPS = FIRST.get(itemName).contains(EPS);
			FIRST.get(itemName).remove(EPS);
			if (isLexerRule(itemName)
					&& (itemName.equals(EPS) && opt.size() != 1)) {
				throw new IllegalStateException(EPS_ERROR);
			}
			a.addAll(FIRST.get(itemName));
			if (isEPS) {
				FIRST.get(itemName).add(EPS);
				a.addAll(calcFirstForOption(opt, pos + 1));
			}
		}
		return a;
	}

	private void initCalc(ArrayList<ParserRule> parserRules, Set<String> tokens) {
		for (ParserRule rule : parserRules) {
			FIRST.put(rule.name, new HashSet<String>());
		}
		for (String token : tokens) {
			Set<String> tmp = new HashSet<String>();
			tmp.add(token);
			FIRST.put(token, tmp);
		}
	}

	private void printRuleClass(ParserRule r) {
		Core.printAtLevel(out, 1, "public class " + buildType(r.name) + " {\n");
		Core.printAtLevel(out, 2, "String text;\n");
		for (String var : r.vars) {
			Core.printAtLevel(out, 2, var + ";\n");
		}
		Core.close(out, 1);
	}

	private void printParseMethod(ParserRule startRule) {
		String returnType = buildType(startRule.name);
		Core.printAtLevel(out, 1, returnType + " parse(String text");
		if (!startRule.args.isEmpty()) {
			out.print(", ");
		}
		printArgs(startRule.args);
		out.println(PAR_THROWS_CB);
		Core.printAtLevel(out, 2, "lex = new " + LEX + "(text);\n");
		Core.printAtLevel(out, 2, "lex.nextToken();\n");
		printCheck(FIRST.get(startRule.name), 2);
		String ctx = ctxName(startRule.name, -1);
		Core.printAtLevel(out, 2, returnType + " " + ctx + " = "
				+ buildInvocation(startRule.name, startRule.args) + ";\n");
		{
			Set<String> tmp = new HashSet<String>();
			tmp.add(EOF);
			printCheck(tmp, 2);
		}
		printReturn(ctx, 2);
		Core.close(out, 1);
	}

	private void printRuleMethod(ParserRule rule) {
		String ctx = rule.name + '0';
		printRuleMethodHeader(rule, buildType(rule.name), ctx);
		out.println(rule.initCode);
		Core.printAtLevel(out, 2, "StringBuilder sb = new StringBuilder();\n");
		Core.printAtLevel(out, 2, "switch(lex.currTokenType()) {\n");
		for (ArrayList<ParseItem> option : rule.options) {
			printSwitchCases(option, 0);
			printConsumeOption(option);
			Core.printAtLevel(out, 3, "break;\n");
		}
		Core.close(out, 2);
		Core.printAtLevel(out, 2, ctx + ".text = sb.toString();\n");
		printReturn(ctx, 2);
		Core.close(out, 1);
	}

	private void printConsumeOption(ArrayList<ParseItem> option) {
		for (int i = 0, n = option.size(); i < n; ++i) {
			ParseItem item = option.get(i);
			printCheck(FIRST.get(item.name), 3);
			printConsumeItem(i, item);
			out.println(item.code);
		}
	}

	private void printConsumeItem(int num, ParseItem item) {
		if (isLexerRule(item.name)) {
			printConsumeToken(item.name, num);
		} else {
			String ctx = ctxName(item.name, num);
			Core.printAtLevel(out, 3, buildType(item.name) + " " + ctx + " = "
					+ item.name + item.args + ";\n");
			Core.printAtLevel(out, 3, "sb.append(" + ctx + ".text);\n");
		}
	}

	private void printConsumeToken(String token, int i) {
		if (!token.equals(EPS)) {
			Core.printAtLevel(out, 3, "sb.append(lex.currToken());\n");
			Core.printAtLevel(out, 3, "String " + ctxName(token, i)
					+ " = consume();\n");
		}
	}

	private String ctxName(String itemName, int i) {
		return itemName + (i + 1);
	}

	private void printCheck(Set<String> tokens, int level) {
		Core.printAtLevel(out, level, "check(");
		int i = 0, n = tokens.size();
		for (String t : tokens) {
			printLexerConst(t);
			if (i < n - 1) {
				out.print(", ");
			}
			++i;
		}
		out.println(");");
	}

	private void printSwitchCases(ArrayList<ParseItem> option, int pos) {
		if (pos == option.size()) {
			Core.printAtLevel(out, 2, "default:\n");
			return;
		}
		String itemName = option.get(pos).name;
		if (isLexerRule(itemName)) {
			if (!itemName.equals(EPS)) {
				printSwitchCase(itemName);
			}
		} else {
			for (String token : FIRST.get(itemName)) {
				printSwitchCase(token);
			}
		}
		if (FIRST.get(itemName).contains(EPS)) {
			printSwitchCases(option, pos + 1);
		}
	}

	private void printSwitchCase(String token) {
		Core.printAtLevel(out, 2, "case ");
		printLexerConst(token);
		out.println(":");
	}

	private void printLexerConst(String s) {
		out.print(LEX + "." + s);
	}

	private boolean isLexerRule(String name) {
		return Character.isUpperCase(name.charAt(0));
	}

	private void printRuleMethodHeader(ParserRule rule, String returnType,
			String ctx) {
		Core.printAtLevel(out, 1, returnType + ' ' + rule.name + '(');
		printArgs(rule.args);
		out.println(PAR_THROWS_CB);
		Core.printAtLevel(out, 2, returnType);
		out.print(' ');
		out.print(ctx);
		out.print(" = new ");
		out.print(returnType);
		out.println("();");
	}

	private void printArgs(ArrayList<String> args) {
		for (int i = 0, n = args.size(); i < n; ++i) {
			out.print(args.get(i));
			if (i < n - 1) {
				out.print(", ");
			}
		}
	}

	private void printReturn(String x, int lvl) {
		Core.printAtLevel(out, lvl, "return " + x + ";\n");
	}

	private String buildInvocation(String name, ArrayList<String> args) {
		StringBuilder sb = new StringBuilder();
		sb.append(name);
		sb.append('(');
		for (int i = 0, n = args.size(); i < n; ++i) {
			sb.append(getArgName(args.get(i)));
			if (i < n - 1) {
				sb.append(", ");
			}
		}
		sb.append(')');
		return sb.toString();
	}

	private String getArgName(String arg) {
		StringTokenizer st = new StringTokenizer(arg);
		String varName = "";
		while (st.hasMoreTokens()) {
			varName = st.nextToken();
		}
		return varName;
	}

	private String buildType(String name) {
		return name + '_';
	}

	private void printImports(String imports) {
		out.println("import java.text.ParseException;");
		out.println(imports);
		out.println();
	}

}