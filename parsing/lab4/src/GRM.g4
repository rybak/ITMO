/* Andrey Rybak, NRU ITMO, 3538 */
grammar GRM;

@header
{
import java.util.*;
import java.io.*;
}
@members
{
    String crop(String x, int n) {
        return x.substring(n, x.length() - n);
    }
}

file returns
[
    String name,
    String imports,
    ArrayList<LexerRule> lexerRules,
    ArrayList<LexerRule> skipRules,
    ArrayList<ParserRule> parserRules
]
:
    {
        $lexerRules = new ArrayList<LexerRule>();
        $skipRules = new ArrayList<LexerRule>();
        $parserRules = new ArrayList<ParserRule>();
        $start = null;
    }
    LexerID { $name = $LexerID.text; }
    START_IMPORT
    code { $imports = $code.r; } 
    START_PARSER
    (parsing {
        $parserRules.add($parsing.r);
    })+
    START_LEXER
    (lexerRule { $lexerRules.add($lexerRule.r); })+
    START_SKIP
    (lexerRule { $skipRules.add($lexerRule.r); })+
    EOF
;

parsing returns [ ParserRule r ]
:
    {
        ArrayList<String> args = new ArrayList<String>();
        ArrayList<String> vars = new ArrayList<String>();
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
    { $r = new ArrayList<String>(); }
    (ArgVar { $r.add(crop($ArgVar.text, 1)); })*
;

vars returns [ ArrayList<String> r ]
:
    { $r = new ArrayList<String>(); }
    (RetVar { $r.add(crop($RetVar.text, 1)); })*
;

code returns [ String r; ]
:
    JCode
    {
        $r = crop($JCode.text, 2);
    }
;

parseExpr returns [ ArrayList<ArrayList<ParseItem>> r ]
:
    { $r = new ArrayList<ArrayList<ParseItem>>(); }
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
        {
            $r.add(new ParseItem($itemID.id, c, $itemID.call));
        }
    )+
;

itemID returns [
    String id,
    String call
]
:
    (
        ParserID {
            $call = "()";
            $id = $ParserID.text;
        }
        (callArgs { $call = crop($callArgs.text, 1); } )?
    )
    |
    (
        LexerID {
            $id = $LexerID.text;
            $call = null;
        }
    )
;

callArgs : CallArgs ;

lexerRule returns [ LexerRule r ]
:
    LexerID COLON token SEMICOLON
    { $r = new LexerRule($LexerID.text, $token.r); }
;

token returns [ String r ]
:
    TOKEN { $r = crop($TOKEN.text, 1); }
;

START_IMPORT : '_IMPORT' ;
START_PARSER : '_PARSER' ;
START_LEXER : '_LEXER' ;
START_SKIP : '_SKIP' ;

ArgVar : '%' ~[\n]+ NL;
RetVar : '^' ~[\n]+ NL;
JCode : '{$' (TOKEN|.)*? '$}' ;
CallArgs : '$(' .*? ')$' ;

fragment SmallLetter : 'a'..'z' ;
fragment BigLetter : 'A'..'Z' ;
fragment Letter : 'A'..'Z' | 'a'..'z' | '_' ;
fragment Digit : '0'..'9';
fragment NL : '\n' ;

ParserID : SmallLetter Letter* ;
LexerID : BigLetter Letter*;
ID : Letter+ ;

fragment JID : Letter (Letter | Digit | '.')* ;

COLON : ':' ;
SEMICOLON : ';' ;
OR : '|' ;

COMMENT : '#' ~[\n\r]* '\n' ;
WS : [\n\r \t]+ -> skip ;

// QUOTE : '"' ;
TOKEN : '"' ~["]+ '"' ;
