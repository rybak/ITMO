import java.io.PrintWriter;
import java.util.*;

public class ParserGenerator {

	private PrintWriter out;
	private String name;

	public ParserGenerator(PrintWriter out, String name) {
		this.out = out;
		this.name = name;
	}

	public void generate(ArrayList<ParserRule> parserRules,
			Map<String, Integer> t) {
		printImports();
		out.println(Core.buildClassHeader(name + Core.P, Core.P));
		printRules(parserRules, t);
		Core.close(out, 0);
		out.close();
	}

	public void generate(ArrayList<LexerRule> lexerRules,
			ArrayList<LexerRule> skipRules) {

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

	}

	private void printRuleClass(ParserRule r) {
		if (r.vars != null) {
			Core.printAtLevel(out, 1, "public class " + buildType(r) + " {\n");
			for (String var : r.vars) {
				Core.printAtLevel(out, 2, var + ";\n");
			}
			Core.close(out, 1);
		}
	}

	private void printParseMethod(ParserRule S, Map<String, Integer> t) {
		printMethodHeader(S, "parse");
		printReturn(buildInvocation(S.name, S.args));
		Core.close(out, 1);
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

	private void printRuleMethod(ParserRule r, Map<String, Integer> t) {
		printMethodHeader(r, r.name);
		
		// TODO Auto-generated method stub

		Core.close(out, 1);
	}

	private void printMethodHeader(ParserRule r, String name) {
		Core.printAtLevel(out, 1, buildType(r) + ' ' + name);
		printArgs(r.args);
	}

	private void printArgs(ArrayList<String> args) {
		// TODO Auto-generated method stub
		out.print('(');
		if (args != null) {
			for (int i = 0, n = args.size(); i < n; ++i) {
				out.print(args.get(i));
				if (i < n - 1) {
					out.print(", ");
				}
			}
		}
		out.println(") {");
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