import java.text.ParseException;


public class Main {

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		String text = "    111 hello 1 bla";
		TestLexer lex = new TestLexer(text);
		try {
			lex.nextToken();
			lex.nextToken();
			lex.nextToken();
			lex.nextToken();
		} catch (ParseException e) {
			int n = e.getErrorOffset();
			System.err.println(text);
			for (int i = 0; i < n - 1; ++i) {
				System.err.print(' ');
			}
			System.err.println('^');
			System.err.println(e.getMessage());
			
			//e.printStackTrace();
		}
	}

}
