/* Andrey Rybak, NRU ITMO, 3538 */
grammar GRM;

@header
{
import java.util.*;
import java.io.*;
}

file
returns [
    String name,
    ArrayList<LexerRule> lexerRules
]
:
    {
        init();
        $lexerRules = new ArrayList<LexerRule>();
    }
    ID { $name = $ID.text; }
    START_PARSER
    parsing+
    START_LEXER
    (lexerRule
        {
            $lexerRules.add($lexerRule.r);
        }
    )+
    EOF
    {
        $lexerRules = lexerRules;
    }
;

parsing
:
    ID
    COLON
    parseExpr
    SEMICOLON
;

parseExpr
:
    ID+ (OR ID*)*
;

lexerRule
returns [
    lexerRule r
]
:
    ID
    COLON
    token
    SEMICOLON
    {
        lexerRules.add(new LexerRule($ID.text, $token.text));
    }
;

token : TOKEN ;

START_PARSER : '_PARSER' ;
START_LEXER : '_LEXER' ;

fragment Letter : 'A'..'Z' | 'a'..'z' | '_' ;
fragment Digit : '0'..'9';
ID : Letter+ ;
JID : Letter (Letter | Digit)* ;

COLON : ':' ;
SEMICOLON : ';' ;
OR : '|' ;

COMMENT : '#' ~[\n\r]* '\n' ;
WS : [\n\r \t]+ -> skip ;

// QUOTE : '"' ;
TOKEN : '"' ~["]+ '"' ;
