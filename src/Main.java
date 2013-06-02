import java.io.*;
import java.util.*;
import org.antlr.v4.runtime.*;

public class Main {

	private String outputPath;
	private String grammarFile;

	public Main(String path, String file) {
		this.outputPath = path;
		this.grammarFile = file;
	}

	public void run() throws FileNotFoundException, IOException {
		ANTLRInputStream is = new ANTLRInputStream(new FileInputStream(
				new File(grammarFile)));
		GRMLexer lexer = new GRMLexer(is);
		TokenStream tokens = new CommonTokenStream(lexer);
		GRMParser parser = new GRMParser(tokens);
		generate(parser.file());
	}

	private void generate(GRMParser.FileContext fc) throws IOException {
		String filename = fc.name;
		PrintWriter out = new PrintWriter(outputPath + filename + "Lexer.java");
		Map<String, Integer> t = new LexerGenerator(out, filename).generate(
				fc.imports, fc.lexerRules, fc.skipRules);
		PrintWriter outParser = new PrintWriter(outputPath + filename
				+ "Parser.java");
		new ParserGenerator(outParser, filename).generate(fc.imports, fc.rules,
				fc.start, fc.parserRules, t);
	}

	public static void main(String[] args) {
		try {
			System.err.println(args[0]);
			new Main(args[0], args[1]).run();
		} catch (Exception e) {
			e.printStackTrace();
			System.err.println("ERROR " + e.getMessage());
		}
	}
}