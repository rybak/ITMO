public class TestLexer extends Lexer {
    public static final int
        H = 0,
        B = 1;

    public static final int
        One = 0,
        Space = 1;

    TestLexer(String text) {
        super(text);
        add("^hello", "H");
        add("^bla", "B");
        addSkip("^1");
        addSkip("^ ");
    }

}
