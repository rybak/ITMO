import java.text.ParseException;
import java.util.*;

public class Main {

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		String text = "int a, *p; matrix **b,\n \t ******s  ;";
		CVarsParser p = new CVarsParser();
		try {
			HashMap<String, ArrayList<String>> vars = new HashMap<String, ArrayList<String>>();
			CVarsParser.s_ c = p.parse(text, vars);
			System.err.println(vars);
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
