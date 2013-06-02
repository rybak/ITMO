
import java.io.PrintWriter;

public class Core {
	static final String L = "Lexer";
	static final String P = "Parser";
	static final String E = " extends ";
	static final String PC = "public class ";

	static String buildClassHeader(String className, String X) {
		return PC + className + E + X + " {";
	}

	static void printAtLevel(PrintWriter out, int level, String x) {
		printTabs(out, level);
		out.print(x);
	}

	static void printTabs(PrintWriter out, int level) {
		for (int i = 0; i < level; ++i) {
			out.print("    ");
		}
	}

	static void close(PrintWriter out, int level) {
		Core.printAtLevel(out, level, "}\n");
		out.println();
	}
}
