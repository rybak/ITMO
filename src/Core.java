import java.io.PrintWriter;

public class Core {

	static void printAtLevel(PrintWriter out, int level, String x) {
		printTabs(out, level);
		out.print(x);
	}

	static void printTabs(PrintWriter out, int level) {
		for (int i = 0; i < level; ++i) {
			out.print("    ");
		}
	}
}
