import java.io.*;
import java.util.*;

public class LexerGenerator {

	private PrintWriter out;
	private String name;

	public LexerGenerator(PrintWriter out, String name) {
		this.out = out;
		this.name = name;
	}

	public Map<String, Integer> generate(String imports, ArrayList<LexerRule> lexerRules,
			ArrayList<LexerRule> skipRules) {
		String className = name + Core.L;
		out.println(imports);
		out.println(Core.buildClassHeader(className, Core.L));
		printConsts(lexerRules);
		printConsts(skipRules);
		printConstructor(className, lexerRules, skipRules);
		Core.close(out, 0);
		out.close();
		Map<String, Integer> t = new HashMap<String, Integer>();
		for (int i = 0; i < lexerRules.size(); ++i) {
			LexerRule r = lexerRules.get(i);
			t.put(r.name, i);
		}
		return t;
	}

	private void printConsts(ArrayList<LexerRule> rules) {
		if (rules.isEmpty()) {
			return;
		}
		Core.printAtLevel(out, 1, "public static final int\n");
		for (int i = 0, n = rules.size(); i < n; ++i) {
			Core.printAtLevel(out, 2, rules.get(i).name + " = " + i);
			if (i < n - 1) {
				out.println(",");
			}
		}
		out.println(";");
		out.println();
	}

	private void printConstructor(String className,
			ArrayList<LexerRule> lexerRules, ArrayList<LexerRule> skipRules) {
		Core.printAtLevel(out, 1, className + "(String text) {\n");
		Core.printAtLevel(out, 2, "super(text);\n");
		printAdds(lexerRules);
		printSkips(skipRules);
		Core.close(out, 1);
		out.println();
	}

	private void printSkips(ArrayList<LexerRule> lexerRules) {
		for (LexerRule r : lexerRules) {
			Core.printAtLevel(out, 2, "addSkip(\"^" + r.regexp + "\");\n");
		}
	}

	private void printAdds(ArrayList<LexerRule> lexerRules) {
		for (LexerRule r : lexerRules) {
			Core.printAtLevel(out, 2, "add(\"^" + r.regexp + "\", \"" + r.name
					+ "\");\n");
		}
	}

}
