structure Tokens = Tokens

type pos = int
type svalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult= (svalue,pos) token

val pos = ref 0
fun eof () = Tokens.EOF(!pos,!pos)
fun error (e,l : int,_) = TextIO.output (TextIO.stdOut, String.concat[
	"line ", (Int.toString l), ": ", e, "\n"
      ])
exception Abort

%%
%header (functor CalcLexFun(structure Tokens: Calc_TOKENS));
alpha=[A-Za-z '_'];
digit=[0-9];
ws = [\ \t];
%%
\n       => (Tokens.SEMI(!pos,!pos));
{ws}+    => (lex());
"sporadic"                   => (Tokens.SPORADIC(!pos,!pos));
"implies"                    => (Tokens.IMPLIES(!pos,!pos));
"tag relation"               => (Tokens.TAGREL(!pos,!pos));
"time delayed by"            => (Tokens.TIMEDELAYEDBY(!pos,!pos));
"delayed by"                 => (Tokens.DELAYEDBY(!pos,!pos));
"on"                         => (Tokens.ON(!pos,!pos));
"filtered by"                => (Tokens.FILTEREDBY(!pos,!pos));
"sustained from"             => (Tokens.SUSTAINEDFROM(!pos,!pos));
"sustained immediately from" => (Tokens.SUSTAINEDFROMIMMEDIATELY(!pos,!pos));
"to"                         => (Tokens.TO(!pos,!pos));
"await"                      => (Tokens.AWAIT(!pos,!pos));
"when not"                   => (Tokens.WHENNOT(!pos,!pos));
"when"                       => (Tokens.WHEN(!pos,!pos));
"every"                      => (Tokens.EVERY(!pos,!pos));
"starting at"                => (Tokens.STARTINGAT(!pos,!pos));
"periodic"                   => (Tokens.PERIODIC(!pos,!pos));
"offset"                     => (Tokens.OFFSET(!pos,!pos));
{alpha}+{digit}*{alpha}*     => (Tokens.ID(yytext,!pos,!pos));
{digit}+                     => (Tokens.NUM (valOf (Int.fromString yytext), !pos, !pos));
"="                          => (Tokens.EQ(!pos,!pos));
"+"                          => (Tokens.PLUS(!pos,!pos));
"*"                          => (Tokens.TIMES(!pos,!pos));
"("                          => (Tokens.LPAR(!pos,!pos));
")"                          => (Tokens.RPAR(!pos,!pos));
","                          => (Tokens.COMMA(!pos,!pos));
"."                          => (Tokens.DOT(!pos,!pos));
"@maxstep"                   => (Tokens.DIR_MAXSTEP(!pos,!pos));
"@minstep"                   => (Tokens.DIR_MINSTEP(!pos,!pos));
"@heuristic"                 => (Tokens.DIR_HEURISTIC(!pos,!pos));
"@dumpres"                   => (Tokens.DIR_DUMPRES(!pos,!pos));
"@prefix strict"             => (Tokens.DIR_RUNPREFIX_STRICT(!pos,!pos));
"@prefix"                    => (Tokens.DIR_RUNPREFIX(!pos,!pos));
"@abort"                     => (raise Abort);
"@run"                       => (Tokens.DIR_RUN(!pos,!pos));
"@step"                      => (Tokens.DIR_RUNSTEP(!pos,!pos));
"@print"                     => (Tokens.DIR_PRINT(!pos,!pos));
"//"[^ \n]*                  => (lex());

_                            => (error ("ignoring bad character " ^ yytext,!pos,!pos);
             lex());

