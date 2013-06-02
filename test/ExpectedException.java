import java.text.ParseException;

@SuppressWarnings("serial")
public class ExpectedException extends ParseException {

	public ExpectedException(Lexer lex, int[] expected, int foundType,
			String found, int position) {
		super(tokensToString(lex, expected) + " expected.\nFound "
				+ foundToString(foundType, found) + ".", position);
	}

	private static String tokensToString(Lexer lex, int[] tokens) {
		StringBuilder sb = new StringBuilder();
		for (int i = 0; i < tokens.length; ++i) {
			sb.append(lex.getTokenName(i));
			if (i < tokens.length - 1) {
				sb.append(" or ");
			}
		}
		sb.setCharAt(0, Character.toUpperCase(sb.charAt(0)));
		return sb.toString();
	}

	private static String foundToString(int foundType, String found) {
		switch (foundType) {
		case Lexer.EOF:
			return "end of file";
		default:
			return '\'' + found + '\'';
		}
	}
}
