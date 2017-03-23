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

%%
%header (functor CalcLexFun(structure Tokens: Calc_TOKENS));
alpha=[A-Za-z '_'];
digit=[0-9];
ws = [\ \t];
%%
\n       => (Tokens.SEMI(!pos,!pos));
{ws}+    => (lex());
"int-clock"                  => (Tokens.TYPEDECL(Int_t, !pos,!pos));
"N-clock"			 => (Tokens.TYPEDECL(Int_t, !pos,!pos));
"rational-clock"		 => (Tokens.TYPEDECL(Rat_t, !pos,!pos));
"Q-clock"			 => (Tokens.TYPEDECL(Rat_t, !pos,!pos));
"unit-clock"			 => (Tokens.TYPEDECL(Unit_t, !pos,!pos));
"U-clock"			 => (Tokens.TYPEDECL(Unit_t, !pos,!pos));
"sporadic"                   => (Tokens.SPORADIC(!pos,!pos));
"implies"                    => (Tokens.IMPLIES(!pos,!pos));
"tag relation"               => (Tokens.TAGREL(!pos,!pos));
"time delayed by"            => (Tokens.TIMEDELAYEDBY(!pos,!pos));
"delayed by"                 => (Tokens.DELAYEDBY(!pos,!pos));
"on"                         => (Tokens.ON(!pos,!pos));
"filtered by"                => (Tokens.FILTEREDBY(!pos,!pos));
"sustained from"             => (Tokens.SUSTAINEDFROM(!pos,!pos));
"sustained immediately from" => (Tokens.SUSTAINEDFROMIMMEDIATELY(!pos,!pos));
"weakly"                     => (Tokens.WEAKLY(!pos,!pos));
"to"                         => (Tokens.TO(!pos,!pos));
"await"                      => (Tokens.AWAIT(!pos,!pos));
"when not"                   => (Tokens.WHENNOT(!pos,!pos));
"when"                       => (Tokens.WHEN(!pos,!pos));
"every"                      => (Tokens.EVERY(!pos,!pos));
"starting at"                => (Tokens.STARTINGAT(!pos,!pos));
"periodic"                   => (Tokens.PERIODIC(!pos,!pos));
"offset"                     => (Tokens.OFFSET(!pos,!pos));
"next"                       => (Tokens.NEXT(!pos,!pos));
"with"                       => (Tokens.WITH(!pos,!pos));
"reset"                      => (Tokens.RESET(!pos,!pos));
{alpha}+{digit}*{alpha}*     => (Tokens.ID(yytext,!pos,!pos));
{digit}+"."{digit}+          => (Tokens.DECNUM (valOf (rat_of_string yytext), !pos, !pos));
{digit}+                     => (Tokens.NUM (valOf (Int.fromString yytext), !pos, !pos));
"="                          => (Tokens.EQ(!pos,!pos));
"+"                          => (Tokens.PLUS(!pos,!pos));
"*"                          => (Tokens.TIMES(!pos,!pos));
"()"                         => (Tokens.UNIT_VAL(!pos,!pos));
"("                          => (Tokens.LPAR(!pos,!pos));
")"                          => (Tokens.RPAR(!pos,!pos));
","                          => (Tokens.COMMA(!pos,!pos));
"@maxstep"                   => (Tokens.DIR_MAXSTEP(!pos,!pos));
"@minstep"                   => (Tokens.DIR_MINSTEP(!pos,!pos));
"@heuristic"                 => (Tokens.DIR_HEURISTIC(!pos,!pos));
"@dumpres"                   => (Tokens.DIR_DUMPRES(!pos,!pos));
"@scenario strict"           => (Tokens.DIR_RUNPREFIX_STRICT(!pos,!pos));
"@scenario"                  => (Tokens.DIR_RUNPREFIX(!pos,!pos));
"@exit"                      => (Tokens.DIR_EXIT(!pos,!pos));
"@run"                       => (Tokens.DIR_RUN(!pos,!pos));
"@step"                      => (Tokens.DIR_RUNSTEP(!pos,!pos));
"@print"                     => (Tokens.DIR_PRINT(!pos,!pos));
"@help"                      => (Tokens.DIR_HELP(!pos,!pos));
"//"[^ \n]*                  => (lex());
.                            => (error ("ignoring bad character " ^ yytext,!pos,!pos);
             lex());

