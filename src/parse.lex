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
"unit-clock"			 => (Tokens.TYPEDECL(Unit_t, !pos,!pos));
"U-clock"			 => (Tokens.TYPEDECL(Unit_t, !pos,!pos));
"int-clock"                  => (Tokens.TYPEDECL(Int_t, !pos,!pos));
"N-clock"			 => (Tokens.TYPEDECL(Int_t, !pos,!pos));
"decimal-clock"		 => (raise UnsupportedParsedTerm);
"D-clock"			 => (raise UnsupportedParsedTerm);
"rational-clock"		 => (Tokens.TYPEDECL(Rat_t, !pos,!pos));
"Q-clock"			 => (Tokens.TYPEDECL(Rat_t, !pos,!pos));
"let"                        => (Tokens.LET(!pos,!pos)); 
"int"				 => (Tokens.INT(!pos,!pos)); 
"decimal"			 => (Tokens.DECIMAL(!pos,!pos)); 
"rational"			 => (Tokens.RATIONAL(!pos,!pos)); 
"float"			 => (Tokens.FLOAT(!pos,!pos)); 
"sporadic"                   => (Tokens.SPORADIC(!pos,!pos));
"implies"                    => (Tokens.IMPLIES(!pos,!pos));
"tag relation"               => (Tokens.TAGREL(!pos,!pos));
"by"                         => (Tokens.BY(!pos,!pos));
"time"                       => (Tokens.TIME(!pos,!pos));
"delayed"                    => (Tokens.DELAYED(!pos,!pos));
"on"                         => (Tokens.ON(!pos,!pos));
"filtered"                   => (Tokens.FILTERED(!pos,!pos));
"from"                       => (Tokens.FROM(!pos,!pos));
"sustained"                  => (Tokens.SUSTAINED(!pos,!pos));
"immediately"                => (Tokens.IMMEDIATELY(!pos,!pos));
"weakly"                     => (Tokens.WEAKLY(!pos,!pos));
"to"                         => (Tokens.TO(!pos,!pos));
"await"                      => (Tokens.AWAIT(!pos,!pos));
"when"                       => (Tokens.WHEN(!pos,!pos));
"not"                        => (Tokens.NOT(!pos,!pos));
"every"                      => (Tokens.EVERY(!pos,!pos));
"at"                         => (Tokens.AT(!pos,!pos));
"starting"                   => (Tokens.STARTING(!pos,!pos));
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
"<"                          => (Tokens.LDIP(!pos,!pos));
">"                          => (Tokens.RDIP(!pos,!pos));
","                          => (Tokens.COMMA(!pos,!pos));
"@maxstep"                   => (Tokens.DIR_MAXSTEP(!pos,!pos));
"@minstep"                   => (Tokens.DIR_MINSTEP(!pos,!pos));
"@heuristic"                 => (Tokens.DIR_HEURISTIC(!pos,!pos));
"@dumpres"                   => (Tokens.DIR_DUMPRES(!pos,!pos));
"strict"                     => (Tokens.STRICT(!pos,!pos));
"@scenario"                  => (Tokens.DIR_RUNPREFIX(!pos,!pos));
"@exit"                      => (Tokens.DIR_EXIT(!pos,!pos));
"@run"                       => (Tokens.DIR_RUN(!pos,!pos));
"@step"                      => (Tokens.DIR_RUNSTEP(!pos,!pos));
"@print"                     => (Tokens.DIR_PRINT(!pos,!pos));
"@help"                      => (Tokens.DIR_HELP(!pos,!pos));
"//"[^ \n]*                  => (lex());
.                            => (error ("ignoring bad character " ^ yytext,!pos,!pos);
             lex());

