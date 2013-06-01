import java.io.*;
import java.util.*;
import java.util.regex.*;
import org.antlr.v4.runtime.*;

public class Main {

	private String name;
	private PrintWriter outParser;

	private final String path = "../parsing-lab4-test/src/"; 
	
	public void run() throws FileNotFoundException, IOException {
		ANTLRInputStream is = new ANTLRInputStream(new FileInputStream(
				new File(path + "test.grm")));
		GRMLexer lexer = new GRMLexer(is);
		TokenStream tokens = new CommonTokenStream(lexer);
		GRMParser parser = new GRMParser(
				(org.antlr.v4.runtime.TokenStream) tokens);
		generate(parser.file());
	}

	private void generate(GRMParser.FileContext fc) throws IOException {
		name = fc.name;
		PrintWriter out = new PrintWriter(new FileWriter(path + name + "Lexer.java"));
		new LexerGenerator(out, name).generate(fc.lexerRules, fc.skipRules);
		outParser = new PrintWriter(new FileWriter(path + name + "Parser.java"));
	}

	public static void main(String[] args) {
		try {
			new Main().run();
		} catch (Exception e) {
			e.printStackTrace();
			System.err.println("ERROR " + e.getMessage());
		}
	}
}