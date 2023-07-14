public class ParseItem {
	public String name;
	public String code;
	public String args;

	public ParseItem(String name, String code, String args) {
		this.name = name;
		this.code = code;
		this.args = args;
	}

	public String toString() {
		return name;
	}
}
