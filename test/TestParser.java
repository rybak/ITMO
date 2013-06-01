import java.text.ParseException;

public class TestParser extends Parser {
    id_ parse(String text,   int b) throws ParseException {
        lex = new TestLexer(text);
        lex.nextToken();
        return id(b);
    }
    public class id_ {
        	int a;
    }
    id_ id(  int b) throws ParseException {
        id_ id0 = new id_();
        return id0;
    }
    void c() throws ParseException {
    }
}
