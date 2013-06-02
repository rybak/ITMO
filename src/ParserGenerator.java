import java.io.PrintWriter;
import java.util.*;

public class ParserGenerator {

	private PrintWriter out;
	private String name;
	private String LEX;
	private static final String PAR_THROWS_CB = ") throws ParseException {";

	public ParserGenerator(PrintWriter out, String name) {
		this.out = out;
		this.name = name;
		LEX = name + Core.L;
	}

	public void generate(ArrayList<ParserRule> parserRules,
			Map<String, Integer> t) {
		printImports();
		out.println(Core.buildClassHeader(name + Core.P, Core.P));
		printRules(parserRules, t);
		Core.close(out, 0);
		out.close();
	}

	private void printRules(ArrayList<ParserRule> parserRules,
			Map<String, Integer> t) {
		calcRules(parserRules);
		printParseMethod(parserRules.get(0), t);
		for (ParserRule r : parserRules) {
			printRuleClass(r);
			printRuleMethod(r, t);
		}
	}

	private Map<String, ArrayList<Integer>> first = new HashMap<String, ArrayList<Integer>>();
	private Map<String, ArrayList<Integer>> follow = new HashMap<String, ArrayList<Integer>>();

	private void calcRules(ArrayList<ParserRule> parserRules) {
		// TODO delete test code VVV
		ArrayList<Integer> test = new ArrayList<Integer>();
		test.add(0);
		for (ParserRule r : parserRules) {
			first.put(r.name, test);
		}
		// TODO delete test code ^^^
	}

	private void printRuleClass(ParserRule r) {
		Core.printAtLevel(out, 1, "public class " + buildType(r) + " {\n");
		for (String var : r.vars) {
			Core.printAtLevel(out, 2, var + ";\n");
		}
		Core.close(out, 1);
	}

	private void printParseMethod(ParserRule S, Map<String, Integer> t) {
		Core.printAtLevel(out, 1, buildType(S) + " parse(String text, ");
		printArgs(S.args);
		out.println(PAR_THROWS_CB);
		Core.printAtLevel(out, 2, "lex = new " + LEX + "(text);\n");
		Core.printAtLevel(out, 2, "lex.nextToken();\n");
		printCheck(first.get(S.name), 2);
		printReturn(buildInvocation(S.name, S.args));
		Core.close(out, 1);
	}

	private void printCheck(ArrayList<Integer> tokens, int level) {
		Core.printAtLevel(out, level, "check(");
		for (int i = 0, n = tokens.size(); i < n; ++i) {
			out.print(tokens.get(i));
			if (i < n - 1) {
				out.print(", ");
			}
		}
		out.println(");");
	}

	private void printRuleMethod(ParserRule r, Map<String, Integer> tokens) {
		String returnType = r.name + '_';
		String ctx = r.name + '0';
		printRuleMethodHeader(r, returnType, ctx);
		out.println(r.initCode);
		Core.printAtLevel(out, 2, "switch(lex.currTokenType()) {\n");
		for (int t : first.get(r.name)) {
			Core.printAtLevel(out, 2, "case " + t + ":\n");
			Core.printAtLevel(out, 3, "\n");
			Core.printAtLevel(out, 3, "break;\n");
		}
		Core.close(out, 2);
		printReturn(ctx);
		Core.close(out, 1);
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

	private void printReturn(String x) {
		Core.printAtLevel(out, 2, "return " + x + ";\n");
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
		st.nextToken();
		return st.nextToken();
	}

	private String buildType(ParserRule r) {
		if (r.vars == null) {
			return "void";
		}
		return r.name + '_';
	}

	private void printImports() {
		out.println("import java.text.ParseException;");
		out.println();
	}

}