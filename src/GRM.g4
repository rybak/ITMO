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
    ArrayList<LexerRule> lexerRules,
    ArrayList<LexerRule> skipRules,
    ArrayList<ParserRule> parserRules
]
:
    {
        $lexerRules = new ArrayList<LexerRule>();
        $skipRules = new ArrayList<LexerRule>();
        $parserRules = new ArrayList<ParserRule>();
    }
    ID { $name = $ID.text; }
    START_PARSER
    (parsing { $parserRules.add($parsing.r); })+
    START_LEXER
    (lexerRule { $lexerRules.add($lexerRule.r); })+
    START_SKIP
    (lexerRule { $skipRules.add($lexerRule.r); })+
    EOF
;

parsing
returns [
    ParserRule r
]
:
    {
        ArrayList<ArrayList<ParseItem>> options = new ArrayList<ArrayList<ParseItem>>();
        ArrayList<String> args = null;
        ArrayList<String> vars = null;
    }
    ParserID
    (args
            {
            args = $args.r;
        }
    )?
    (vars
        {
            vars = $vars.r;
        }
    )?
    COLON
    parseExpr
    SEMICOLON
    {
        $r = new ParserRule($ParserID.text, args, vars, $code.text, options);
    }
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
        $r = new LexerRule($ID.text, $token.text);
    }
;

token : TOKEN ;

START_PARSER : '_PARSER' ;
START_LEXER : '_LEXER' ;
START_SKIP : '_SKIP' ;

fragment SmallLetter : 'a'..'z' ;
fragment BigLetter : 'A'..'Z' ;
fragment Letter : 'A'..'Z' | 'a'..'z' | '_' ;
fragment Digit : '0'..'9';

fragment JMark : '#' ;
JVar : JMark JID JID ;
fragment JID : Letter (Letter | Digit | '.')* ;
JCode : JMark ~[\n]* '\n' ;
ParserID : SmallLetter Letter* ;
LexerID : BigLetter Letter*;
ID : Letter+ ;

COLON : ':' ;
SEMICOLON : ';' ;
OR : '|' ;

COMMENT : '#' ~[\n\r]* '\n' ;
WS : [\n\r \t]+ -> skip ;

// QUOTE : '"' ;
TOKEN : '"' ~["]+ '"' ;
