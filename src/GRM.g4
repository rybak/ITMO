/* Andrey Rybak, NRU ITMO, 3538 */
grammar GRM;

@header
{
import java.util.*;
import java.io.*;
}

file returns
[
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

parsing returns [ ParserRule r ]
:
    {
        ArrayList<String> args = null;
        ArrayList<String> vars = null;
        String ic = "";
    }
    ParserID
    (args { args = $args.r; })?
    (vars { vars = $vars.r; })?
    COLON
    (code { ic = $code.r; })?
    parseExpr
    SEMICOLON
    { $r = new ParserRule($ParserID.text, args, vars, ic, $parseExpr.r); }
;

args returns [ ArrayList<String> r ]
:
    LB
    { $r = new ArrayList<String>(); }
    (JVar { $r.add(JVar.text); })+
    RB
;

vars returns [ ArrayList<String> r ]
:
    VARS_B
    { $r = new ArrayList<String>(); }
    (JVar { $r.add(JVar.text); })+
    RB
;

code returns [ String r; ]
:
    LCB
    { ArrayList<String> a = new ArrayList<String>(); }
    ( JCode { a.add(JCode.subString(1); })+
    {
        StringBuilder sb = new StringBuilder();
        for (String s : a) {
            sb.append(s);
            sb.append('\n');
        }
        $r = sb.toString();
    }
    RCB
;

parseExpr returns [ ArrayList<ArrayList<ParseItem>> r ]
:
    { r = new ArrayList<ArrayList<ParseItem>(); }
    parseOption { $r.add($parseOption.r); }
    (OR parseOption { $r.add($parseOption.r); })*
;

parseOption returns [ ArrayList<ParseItem> r ]
:
    { $r = new ArrayList<ParseItem>(); }
    (
        { String c = ""; }
        itemID
        (code { c = $code.r; })?
        { $r.add(new ParseItem($itemID.text, c); }
    )+
;

itemID : ParserID | LexerID ;

lexerRule returns [ lexerRule r ]
:
    LexerID COLON token SEMICOLON
    { $r = new LexerRule($LexerID.text, $token.text); }
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
ParserID : SmallLetter Letter* ;
LexerID : BigLetter Letter*;
ID : Letter+ ;

JVar : JMark JID JID ;
fragment JID : Letter (Letter | Digit | '.')* ;
JCode : JMark ~[\n]* '\n' ;

LCB : '{\n' ;
RCB : '}\n' ;
VARS_B : 's[\n';
LB : '[\n' ;
RB : ']\n' ;

COLON : ':' ;
SEMICOLON : ';' ;
OR : '|' ;

COMMENT : '#' ~[\n\r]* '\n' ;
WS : [\n\r \t]+ -> skip ;

// QUOTE : '"' ;
TOKEN : '"' ~["]+ '"' ;
