import java.util.*;

public class ParserRule {

	public final String name;
	public final ArrayList<String> args;
	public final ArrayList<String> vars;
	public final String initCode;
	public final ArrayList<ArrayList<ParseItem>> options;

	public ParserRule(String name, ArrayList<String> args, ArrayList<String> sVars, String initCode,
			ArrayList<ArrayList<ParseItem>> options) {
		this.name = name;
		this.args = args;
		this.vars = sVars;
		this.initCode = initCode;
		this.options = options;
	}

}
