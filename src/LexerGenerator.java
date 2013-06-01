import java.io.*;
import java.util.*;

public class LexerGenerator {

	private PrintWriter out;
	private String name;

	public LexerGenerator(PrintWriter out, String name) {
		this.out = out;
		this.name = name;
	}

	private static final String LEX = "Lexer";

	public void generate(ArrayList<LexerRule> lexerRules,
			ArrayList<LexerRule> skipRules) {
		printImports();
		String className = name + LEX;
		out.println("class " + className + " extends " + LEX + " {");
		
		printConsts(lexerRules);
		printConsts(skipRules);

		// printFields();
		printConstructor(className, lexerRules, skipRules);
		// printMethods();
		out.println("}");
		out.close();
	}

	private void printMethods() {
		printCurrTokenTypeMethod();
		printNextTokenMethod();
	}

	private void printCurrTokenTypeMethod() {
		Core.printAtLevel(out, 1, "int currTokenType() {\n");
		Core.printAtLevel(out, 2, "return currTokenType;\n");
		Core.printAtLevel(out, 1, "}\n");
	}

	private void printNextTokenMethod() {
		// TODO Auto-generated method stub

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
		// Core.printAtLevel(out, 1,
		// "public static final String[] tokenRegexp = {\n");
		// for (int i = 0, n = lexerRules.size(); i < n; ++i) {
		// Core.printAtLevel(out, 2, lexerRules.get(i).regexp);
		// if (i < n - 1) {
		// out.println(",");
		// } else {
		// out.println();
		// }
		// }
		// Core.printAtLevel(out, 1, "};");
		// out.println();
		// out.println();

	}

	private void printFields() {
		Core.printAtLevel(out, 1, "private String text;\n");
		Core.printAtLevel(out, 1, "private int pos;\n");
		Core.printAtLevel(out, 1, "private int currChar;\n");
		Core.printAtLevel(out, 1, "private int currTokenType;\n");
		Core.printAtLevel(out, 1, "private String currToken;\n");
		out.println();
	}

	private void printConstructor(String className, ArrayList<LexerRule> lexerRules, ArrayList<LexerRule> skipRules) {
		Core.printAtLevel(out, 1, className + "(String text) {\n");
		Core.printAtLevel(out, 2, "super(text);\n");
		printAdds(lexerRules, "add");
		printAdds(skipRules, "addSkip");
		Core.printAtLevel(out, 1, "}\n");
		out.println();
	}

	private void printAdds(ArrayList<LexerRule> lexerRules, String add) {
		for (int i = 0, n = lexerRules.size(); i < n; ++i) {
			Core.printAtLevel(out, 2, add + "(\"^" + lexerRules.get(i).regexp + "\");\n");
		}
	}

	private void printImports() {
		out.println("import java.text.ParseException;");
		out.println("import java.util.regex.*;");
		out.println();
	}

}
