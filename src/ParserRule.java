import java.util.*;

public class ParserRule {

	final String name;
	final ArrayList<String> args;
	final ArrayList<String> vars;
	final String initCode;
	final ArrayList<ArrayList<ParseItem>> options;
	final int cnt;

	boolean isEPS;
	
	public ParserRule(String name, ArrayList<String> args,
			ArrayList<String> sVars, String initCode,
			ArrayList<ArrayList<ParseItem>> options) {
		this.name = name;
		this.args = args;
		this.vars = sVars;
		this.initCode = initCode;
		this.options = options;
		cnt = options.size();
	}

}
