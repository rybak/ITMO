import java.text.ParseException;
import java.util.*;
import java.util.regex.*;

public abstract class Lexer {

	private static final String LEX_ERR = "Lexer error : ";
	public static final int EPS = -3;
	public static final int EOF = -2;
	public static final int INVALID = -1;

	private String text;
	private int pos;
	private final int len;

	private int currTokenType;
	private String currToken;

	private ArrayList<Pattern> keeps;
	private ArrayList<Pattern> skips;
	private ArrayList<String> names;

	public Lexer(String text) {
		this.text = text;
		len = text.length();
		pos = 0;
		currTokenType = INVALID;
		keeps = new ArrayList<Pattern>();
		skips = new ArrayList<Pattern>();
		names = new ArrayList<String>();
	}

	public int currTokenType() {
		return currTokenType;
	}

	public String currToken() {
		return currToken;
	}

	public void nextToken() throws ParseException {
		if (currTokenType == EOF) {
			throw new ParseException("End has already been reached.", pos);
		}
		if (pos == len) {
			currTokenType = EOF;
			return;
		}
		skip();
		if (!match()) {
			currTokenType = INVALID;
			throw new ParseException(LEX_ERR + "illegal token found", pos);
		} else {
			skip();
		}
	}

	private boolean match() {
		return match(keeps, true);
	}

	private void skip() {
		boolean skipping = true;
		while (skipping && pos < len) {
			skipping = match(skips, false);
		}
	}

	private boolean match(ArrayList<Pattern> patterns, boolean keep) {
		for (int i = 0, n = patterns.size(); i < n; ++i) {
			Pattern p = patterns.get(i);
			Matcher m = p.matcher(text.substring(pos));
			if (m.find()) {
				if (keep) {
					currToken = m.group();
					currTokenType = i;
				}
				pos += m.end();
				return true;
			}
		}
		return false;
	}

	protected void add(String regexp, String name) {
		keeps.add(Pattern.compile(regexp));
		names.add(name);
	}

	protected void addSkip(String regexp) {
		skips.add(Pattern.compile(regexp));
	}

	public String getTokenName(int i) {
		return names.get(i);
	}

	public int position() {
		return pos;
	}
}
