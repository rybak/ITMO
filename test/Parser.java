import java.text.ParseException;

public abstract class Parser {
	protected Lexer lex;

	protected void initParse(String text) {
	}

	protected String getToken() throws ParseException {
		String token = lex.currToken();
		lex.nextToken();
		return token;
	}

	protected void check(int... expected) throws ExpectedException {
		int foundType = lex.currTokenType();
		for (int t : expected) {
			if (t == Lexer.EPS) {
				return;
			}
			if (foundType == t) {
				return;
			}
		}
		throw new ExpectedException(lex, expected, foundType, lex.currToken(),
				lex.position());
	}

	protected String consume() throws ParseException {
		String t = lex.currToken();
		lex.nextToken();
		return t;
	}
}
