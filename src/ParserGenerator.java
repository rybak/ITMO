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

	public void generate(String imports, Map<String, ParserRule> rules,
			String start, ArrayList<ParserRule> parserRules,
			Map<String, Integer> tokens) {
		tokens.put(EPS, -3);
		printImports(imports);
		out.println(Core.buildClassHeader(name + Core.P, Core.P));
		calcRules(rules, parserRules, tokens);
		printParseMethod(parserRules.get(0), tokens);
		printRules(rules, parserRules, tokens);
		Core.close(out, 0);
		out.close();
	}

	private void printRules(Map<String, ParserRule> rules,
			ArrayList<ParserRule> parserRules, Map<String, Integer> tokens) {
		for (ParserRule r : parserRules) {
			printRuleClass(r);
			printRuleMethod(rules, r, tokens);
		}
	}

	private Map<String, Set<String>> first = new HashMap<String, Set<String>>();
	private Map<String, Set<String>> follow = new HashMap<String, Set<String>>();

	private void calcRules(Map<String, ParserRule> rules,
			ArrayList<ParserRule> parserRules, Map<String, Integer> tokens) {
		initCalc(parserRules, tokens);
		calcFirst(rules, parserRules);
		System.out.println(first.toString());
	}

	private void calcFirst(Map<String, ParserRule> rules,
			ArrayList<ParserRule> parserRules) {
		boolean changes = true;
		while (changes) {
			changes = false;
			for (ParserRule rule : parserRules) {
				int oldSize = first.get(rule.name).size();
				first.get(rule.name).addAll(calcFirstForRule(rules, rule));
				int newSize = first.get(rule.name).size();
				if (newSize != oldSize) {
					changes = true;
				}
			}
		}
		if (parserRules.get(0).isEPS) {
			first.get(parserRules.get(0).name).add(EOF);
		}
	}

	private Set<String> calcFirstForRule(Map<String, ParserRule> rules,
			ParserRule rule) {
		Set<String> a = new HashSet<String>();
		for (ArrayList<ParseItem> opt : rule.options) {
			a.addAll(calcFirstForOption(rules, opt, 0));
		}
		if (a.contains(EPS)) {
			rule.isEPS = true;
		}
		return a;
	}

	private Set<String> calcFirstForOption(Map<String, ParserRule> rules,
			ArrayList<ParseItem> opt, int pos) {
		Set<String> a = new HashSet<String>();
		if (pos == opt.size()) {
			a.add(EPS);
		} else {
			String itemName = opt.get(pos).name;
			if (isLexerRule(itemName)) {
				if (itemName.equals(EPS)) {
					if (opt.size() != 1) {
						throw new IllegalStateException(EPS_ERROR);
					}
				}
				a.add(itemName);
			} else {
				a.addAll(first.get(itemName));
				if (first.get(itemName).contains(EPS)) {
					a.addAll(calcFirstForOption(rules, opt, pos + 1));
				}
			}
		}
		return a;
	}

	private void initCalc(ArrayList<ParserRule> parserRules,
			Map<String, Integer> tokens) {
		for (ParserRule rule : parserRules) {
			first.put(rule.name, new HashSet<String>());
			follow.put(rule.name, new HashSet<String>());
		}
		follow.get(parserRules.get(0).name).add(EOF);
		for (String token : tokens.keySet()) {
			Set<String> tmp = new HashSet<String>();
			tmp.add(token);
			first.put(token, tmp);
		}
	}

	private void printRuleClass(ParserRule r) {
		Core.printAtLevel(out, 1, "public class " + buildType(r.name) + " {\n");
		Core.printAtLevel(out, 2, "String text;\n");
		System.err.println("printClass : " + r.name);
		for (String var : r.vars) {
			System.err.println("\n ...");
			Core.printAtLevel(out, 2, var + ";\n");
		}
		Core.close(out, 1);
	}

	private void printParseMethod(ParserRule S, Map<String, Integer> t) {
		Core.printAtLevel(out, 1, buildType(S.name) + " parse(String text");
		if (!S.args.isEmpty()) {
			out.print(", ");
		}
		printArgs(S.args);
		out.println(PAR_THROWS_CB);
		Core.printAtLevel(out, 2, "lex = new " + LEX + "(text);\n");
		Core.printAtLevel(out, 2, "lex.nextToken();\n");
		printCheck(first.get(S.name), 2);
		String ctx = S.name + '0';
		Core.printAtLevel(out, 2, buildType(S.name) + " " + ctx + " = "
				+ buildInvocation(S.name, S.args) + ";\n");
		printCheck(follow.get(S.name), 2);
		printReturn(ctx, 2);
		Core.close(out, 1);
	}

	private void printRuleMethod(Map<String, ParserRule> rules, ParserRule r,
			Map<String, Integer> tokens) {
		String returnType = r.name + '_';
		String ctx = r.name + '0';
		printRuleMethodHeader(r, returnType, ctx);
		out.println(r.initCode);
		Core.printAtLevel(out, 2, "StringBuilder sb = new StringBuilder();\n");
		Core.printAtLevel(out, 2, "switch(lex.currTokenType()) {\n");
		for (ArrayList<ParseItem> option : r.options) {
			printCaseTokens(rules, option, 0);
			printConsumeOption(rules, r, tokens, option);
			Core.printAtLevel(out, 3, "break;\n");
		}
		Core.close(out, 2);
		Core.printAtLevel(out, 2, ctx + ".text = sb.toString();\n");
		printReturn(ctx, 2);
		Core.close(out, 1);
	}

	private void printConsumeOption(Map<String, ParserRule> rules,
			ParserRule r, Map<String, Integer> tokens,
			ArrayList<ParseItem> option) {
		for (int i = 0, n = option.size(); i < n; ++i) {
			ParseItem item = option.get(i);
			if (!first.get(item.name).contains(EPS)) {
				printCheck(first.get(item.name), 3);
			}
			if (isLexerRule(item.name)) {
				if (!option.get(i).name.equals(EPS)) {
					printConsumeToken(item.name, i);
				}
			} else {
				String ctx = ctxName(item.name, i);
				Core.printAtLevel(out, 3, buildType(item.name) + " " + ctx
						+ " = " + item.name + item.args + ";\n");
				Core.printAtLevel(out, 3, "sb.append(" + ctx + ".text);\n");
			}
			out.println(item.code);
		}
	}

	private void printConsumeToken(String token, int i) {
		Core.printAtLevel(out, 3, "sb.append(lex.currToken());\n");
		Core.printAtLevel(out, 3, "String " + ctxName(token, i)
				+ " = consume();\n");
	}

	private String ctxName(String itemName, int i) {
		return itemName + (i + 1);
	}

	private static final String CHK = "check(";

	private void printCheck(Set<String> tokens, int level) {
		Core.printAtLevel(out, level, CHK);
		int i = 0, n = tokens.size();
		for (String t : tokens) {
			if (t.equals(EPS)) {
			} else {
				printLexerConst(t);
				if (i < n - 1) {
					out.print(", ");
				}
			}
			++i;
		}
		out.println(");");
	}

	private void printCaseTokens(Map<String, ParserRule> rules,
			ArrayList<ParseItem> opt, int pos) {
		if (pos == opt.size()) {
			Core.printAtLevel(out, 2, "default:\n");
			return;
		}
		String itemName = opt.get(pos).name;
		if (isLexerRule(itemName)) {
			if (!itemName.equals(EPS)) {
				printSwitchCase(itemName);
			} else {
				printCaseTokens(rules, opt, pos + 1);
			}
		} else {
			for (String token : first.get(itemName)) {
				printSwitchCase(token);
			}
			if (first.get(itemName).contains(EPS)) {
				printCaseTokens(rules, opt, pos + 1);
			}
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

	private void printRuleMethodHeader(ParserRule r, String returnType,
			String ctx) {
		Core.printAtLevel(out, 1, returnType + ' ' + r.name + '(');
		printArgs(r.args);
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
		if (args != null) {
			for (int i = 0, n = args.size(); i < n; ++i) {
				sb.append(getArgName(args.get(i)));
				if (i < n - 1) {
					sb.append(", ");
				}
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