import java.io.*;
import java.util.*;
import org.antlr.v4.runtime.*;

public class Main {

	private String name;
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
		PrintWriter out = new PrintWriter(new FileWriter(path + name
				+ "Lexer.java"));
		Map<String, Integer> t = new LexerGenerator(out, name).generate(
				fc.lexerRules, fc.skipRules);
		PrintWriter outParser = new PrintWriter(new FileWriter(path + name
				+ "Parser.java"));
		new ParserGenerator(outParser, name).generate(fc.parserRules, t);
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