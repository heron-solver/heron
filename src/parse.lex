(**
   Module Parse.lex

   Author : Hai Nguyen Van
            LRI, Université Paris-Sud/CNRS
   
   The copyright to this code is held by Laboratoire de Recherche en
   Informatique, Université Paris-Sud/CNRS. All rights reserved. This
   file is distributed under the MIT License.
*)

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
alpha=[A-Za-z];
digit=[0-9];
optsign=("+"|"-")?;
ws = [\ \t];
%%
\n       => (Tokens.SEMI(!pos,!pos));
{ws}+    => (lex());
"unit-clock"			 => (Tokens.TYPEDECL(Unit_t, !pos,!pos));
"U-clock"			 => (Tokens.TYPEDECL(Unit_t, !pos,!pos));
"int-clock"                  => (Tokens.TYPEDECL(Int_t, !pos,!pos));
"Z-clock"			 => (Tokens.TYPEDECL(Int_t, !pos,!pos));
"decimal-clock"		 => (print (BOLD_COLOR ^ YELLOW_COLOR ^ "### WARNING: Decimal numbers are unsupported. Casting type to [rational].\n" ^ RESET_COLOR); Tokens.TYPEDECL(Rat_t, !pos,!pos));
"D-clock"			 => (print (BOLD_COLOR ^ YELLOW_COLOR ^ "### WARNING: Decimal numbers are unsupported. Casting type to [rational].\n" ^ RESET_COLOR); Tokens.TYPEDECL(Rat_t, !pos,!pos));
"rational-clock"		 => (Tokens.TYPEDECL(Rat_t, !pos,!pos));
"Q-clock"			 => (Tokens.TYPEDECL(Rat_t, !pos,!pos));
"let"                        => (Tokens.LET(!pos,!pos)); 
"int"				 => (Tokens.INT(!pos,!pos)); 
"decimal"			 => (Tokens.DECIMAL(!pos,!pos)); 
"rational"			 => (Tokens.RATIONAL(!pos,!pos)); 
"float"			 => (Tokens.FLOAT(!pos,!pos)); 
"sporadic"                   => (Tokens.SPORADIC(!pos,!pos));
"implies"                    => (Tokens.IMPLIES(!pos,!pos));
"time relation"               => (Tokens.TAGREL(!pos,!pos));
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
"strictly"                   => (Tokens.STRICTLY(!pos,!pos));
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
"precedes"                   => (Tokens.PRECEDES(!pos,!pos));
"excludes"                   => (Tokens.EXCLUDES(!pos,!pos));
"kills"                      => (Tokens.KILLS(!pos,!pos));
"->"                         => (Tokens.ARROW(!pos,!pos));
"="                          => (Tokens.EQ(!pos,!pos));
"+"                          => (Tokens.PLUS(!pos,!pos));
"-"                          => (Tokens.MINUS(!pos,!pos));
"*"                          => (Tokens.TIMES(!pos,!pos));
"/"                          => (Tokens.DIV(!pos,!pos));
"()"                         => (Tokens.UNIT_VAL(!pos,!pos));
"("                          => (Tokens.LPAR(!pos,!pos));
")"                          => (Tokens.RPAR(!pos,!pos));
"<"                          => (Tokens.LDIP(!pos,!pos));
">"                          => (Tokens.RDIP(!pos,!pos));
","                          => (Tokens.COMMA(!pos,!pos));
"@maxstep"                   => (Tokens.DIR_MAXSTEP(!pos,!pos));
"@minstep"                   => (Tokens.DIR_MINSTEP(!pos,!pos));
"@policy"                    => (Tokens.DIR_HEURISTIC(!pos,!pos));
"@dumpres"                   => (Tokens.DIR_DUMPRES(!pos,!pos));
"strict"                     => (Tokens.STRICT(!pos,!pos));
"@scenario"                  => (Tokens.DIR_SCENARIO(!pos,!pos));
"@exit"                      => (Tokens.DIR_EXIT(!pos,!pos));
"@run"                       => (Tokens.DIR_RUN(!pos,!pos));
"@step"                      => (Tokens.DIR_RUNSTEP(!pos,!pos));
"@print"                     => (Tokens.DIR_PRINT(!pos,!pos));
"@help"                      => (Tokens.DIR_HELP(!pos,!pos));
"@tagref"			 => (raise UnsupportedParsedTerm);
"@output"                    => (Tokens.DIR_OUTPUT(!pos,!pos));
"@driving-clock"             => (Tokens.DIR_DRIVINGDECL(!pos,!pos));
"@event-concretize"          => (Tokens.DIR_EVENTCONCRETIZE(!pos,!pos));
"@event-solve"               => (Tokens.DIR_EVENTCONCRETIZE(!pos,!pos));
"vcd"				 => (Tokens.VCD(!pos,!pos));
"tikz"				 => (Tokens.TIKZ(!pos,!pos));
"tex"				 => (Tokens.TEX(!pos,!pos));
"svg"				 => (Tokens.SVG(!pos,!pos));
"@select"                    => (Tokens.DIR_SELECT(!pos,!pos));
"select"                     => (Tokens.SELECT(!pos,!pos));
{alpha}({alpha}|{digit}|"_"|"-"|"'")* => (Tokens.ID(yytext,!pos,!pos));
{optsign}{digit}+"."{digit}+ => (Tokens.DECNUM (valOf (rat_of_string yytext), !pos, !pos));
{optsign}{digit}+"."         => (Tokens.DECNUM (valOf (rat_of_string yytext), !pos, !pos));
{optsign}        "."{digit}+ => (Tokens.DECNUM (valOf (rat_of_string yytext), !pos, !pos));
{optsign}{digit}+            => (Tokens.NUM (valOf (Int.fromString yytext), !pos, !pos));
"//"[^ \n]*                  => (lex());
"/*"[^ ]*"*/"                => (lex());
.                            => (error ("ignoring bad character " ^ yytext,!pos,!pos);
             lex());

