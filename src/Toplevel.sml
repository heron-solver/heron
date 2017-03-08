open OS.Process

(* Structures used for lexing/parsing *)
structure CalcLrVals =
  CalcLrValsFun(structure Token = LrParser.Token)

structure CalcLex =
  CalcLexFun(structure Tokens = CalcLrVals.Tokens)

structure CalcParser =
  Join(structure LrParser = LrParser
 structure ParserData = CalcLrVals.ParserData
 structure Lex = CalcLex)

fun invoke lexstream =
    let fun print_error (s,i:int,_) =
      TextIO.output(TextIO.stdOut,
		    "Error, line " ^ (Int.toString i) ^ ", " ^ s ^ "\n")
     in CalcParser.parse(0,lexstream,print_error,())
    end


val maxstep                          = ref ~1
val minstep                          = ref ~1
val heuristics: TESL_atomic list ref = ref []
val dumpres                          = ref false

val prefix: system ref = ref []
val prefix_strict: system ref = ref []

val declared_clocks: clock list ref = ref []

val snapshots: TESL_ARS_conf list ref = ref [([], 0, [], [])]

fun action (stmt: TESL_atomic) = case stmt of
    DirMinstep n	     => minstep := n
  | DirMaxstep n	     => maxstep := n
  | DirHeuristic _	     => heuristics := !heuristics @ [stmt]
  | DirDumpres	     => dumpres := true
  | DirRunprefixStrict (n_step, clocks_prefix) =>
    let val add_haa_constrs =
      List.map (fn c =>
      if List.exists (fn x => x = c) clocks_prefix
      then Ticks (c, n_step)
      else NotTicks (c, n_step)
      ) (!declared_clocks)
      in prefix_strict := (!prefix_strict) @ add_haa_constrs end
  | DirRunprefix (n_step, prefix_clocks) =>
      prefix := (!prefix) @ (List.map (fn c => Ticks (c, n_step)) prefix_clocks)
  | DirRun		     =>
      snapshots := exec (!snapshots) (!minstep, !maxstep, !dumpres, !prefix_strict @ !prefix, !heuristics)
  | DirRunStep	     =>
      let val current_step = case List.hd (!snapshots) of (_, n, _, _) => n + 1
      in snapshots := exec_step (!snapshots) current_step (!minstep, !maxstep, !dumpres, !prefix_strict @ !prefix, !heuristics) end
  | _                     =>
    (snapshots := List.map (fn (G, n, phi, psi) => (G, n, unsugar (phi @ [stmt]), psi)) (!snapshots);
     declared_clocks := uniq ((!declared_clocks) @ (clocks_of_tesl_formula [stmt])))

(* Main REPL *)
fun toplevel () = 
  let val lexer = CalcParser.makeLexer (fn _ =>
						   (case TextIO.inputLine TextIO.stdIn
						    of SOME s => s
						     | _ => ""))
  val dummyEOF = CalcLrVals.Tokens.EOF(0,0)
  val dummySEMI = CalcLrVals.Tokens.SEMI(0,0)
  fun loop lexer =
    let
	 val _ = print "> "
	 val (result,lexer) = invoke lexer
	 val (nextToken,lexer) = CalcParser.Stream.get lexer
	 val _ = case result
		   of SOME stmt =>
		      let val _ = action stmt in
			TextIO.output(TextIO.stdOut, "val it = " ^ (string_of_expr stmt) ^ "\n") end
		     | NONE => ()
	in if CalcParser.sameToken(nextToken,dummyEOF) then (OS.Process.exit OS.Process.success)
	  else loop lexer
      end
     in loop lexer
  end

(* Entry-point *)
val _ = (
  print "Heron 0.1 Release\n";
  toplevel()
)
