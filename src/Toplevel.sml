val RELEASE_VERSION = "0.35.1-alpha+20170327"

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

fun print_help () = (
print (BOLD_COLOR ^ "Heron\n" ^ RESET_COLOR); 
print (BOLD_COLOR ^ "Interactive Simulation Engine for Timed Causality Models in the Tagged Events Specification Language\n" ^ RESET_COLOR); 
print "\n";
print "Copyright (c) 2017, Hai Nguyen Van\n";
print "              Universit\195\169 Paris-Sud / CNRS\n";
(*
print "Please cite:";
print "\n";
*)
print "\n";
print (BOLD_COLOR ^ "TESL language expressions:\n" ^ RESET_COLOR); 
print "  [ID] sporadic [TAG]+\n"; 
print "  [ID] periodic [TAG] (offset [TAG])\n"; 
print "  [ID] implies [ID]\n"; 
print "  tag relation [ID] = [TAG] * [ID] + [TAG]\n"; 
print "  [ID] time delayed by [TAG] on [ID] implies [ID]\n"; 
print "  [ID] delayed by [NAT] on [ID] implies [ID]\n"; 
print "  [ID] filtered by [NAT], [NAT] ([NAT], [NAT])* implies [ID]\n"; 
print "  [ID] every [NAT] implies [ID]\n";
print "  [ID] sustained (immediately) from [ID] to [ID] (weakly) implies [ID]\n"; 
print "  [ID] next to [ID] implies [ID]\n"; 
print "  await [ID]+ implies [ID]\n"; 
print "  [ID] when (not) [ID] implies [ID]\n"; 
print "\n"; 
print "  For more information about the TESL language:\n"; 
print "  http://wwwdi.supelec.fr/software/TESL\n"; 
print "\n"; 
print (BOLD_COLOR ^ "Run parameters:\n" ^ RESET_COLOR);  
print "  @minstep [NAT]                 define the number of minimum run steps\n"; 
print "  @maxstep [NAT]                 define the number of maximum run steps\n"; 
print "  @heuristic [NAME]+             load heuristic(s) among:\n";
print "                                   \u001B[1mall\u001B[0m\n"; 
print "                                   \u001B[1mminimize_floating_ticks\u001B[0m\n";
print "                                   \u001B[1mno_spurious_sporadics\u001B[0m\n";
print "                                   \u001B[1mno_spurious_whentickings\u001B[0m\n";
print "                                   \u001B[1mno_empty_instants\u001B[0m\n";
print "                                   \u001B[1mminimize_sporadic_floating_ticks\u001B[0m\n";
print "                                   \u001B[1mmaximize_reactiveness\u001B[0m\n";
print "  @dumpres                       option to display the results after @run\n"; 
print "  @scenario (strict) [NAT] [ID]+ refine snapshots with instantaneous scenario\n"; 
print "  @scenario (strict) next [ID]+  refine snapshots of next simulation step\n"; 
print "\n"; 
print (BOLD_COLOR ^ "Interactive commands:\n" ^ RESET_COLOR);  
print "  @exit                          exit Heron\n"; 
print "  @run                           run the specification until model found\n"; 
print "  @step                          run the specification for one step\n"; 
print "  @print                         display the current snapshots\n"; 
print "  @help                          display the list of commands\n")

val maxstep                          = ref ~1
val minstep                          = ref ~1
val heuristics: TESL_atomic list ref = ref []
val dumpres                          = ref false

val prefix: system ref = ref []
val prefix_strict: system ref = ref []

val declared_clocks: clock list ref = ref []

val snapshots: TESL_ARS_conf list ref = ref [([], 0, [], [])]
val current_step: int ref = ref 1

val clock_types: (clock * tag_t) list ref = ref []

fun quit () = case (!snapshots) of
    [] => OS.Process.exit OS.Process.failure
  | _  => OS.Process.exit OS.Process.success

fun action (stmt: TESL_atomic) =
  (* On-the-fly clock identifiers declaration *)
  let
      val _ = declared_clocks := uniq ((!declared_clocks) @ (clocks_of_tesl_formula [stmt]))
      val _ = clk_type_declare stmt clock_types
      val _ = type_check (!clock_types)
  in
  case stmt of
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
  | DirRunprefixNextStep (clocks_prefix) => action (DirRunprefix (!current_step, clocks_prefix))
  | DirRunprefixStrictNextStep (clocks_prefix) => action (DirRunprefixStrict (!current_step, clocks_prefix))
  | DirRun		     =>
      snapshots := exec
			  (!snapshots)
			  current_step
			  (!declared_clocks)
			  (!minstep, !maxstep, !dumpres, !prefix_strict @ !prefix, !heuristics)
  | DirRunStep	     =>
      snapshots := exec_step
			  (!snapshots)
			  current_step
			  (!declared_clocks)
			  (!minstep, !maxstep, !dumpres, !prefix_strict @ !prefix, !heuristics)
  | DirPrint              => print_dumpres (!declared_clocks) (!snapshots)
  | DirExit               => quit()
  | DirHelp               => print_help()
  | _                     =>
    snapshots := List.map (fn (G, n, phi, psi) => (G, n, unsugar (!clock_types) (phi @ [stmt]), psi)) (!snapshots)
  end
  handle
  TagTypeInconsistency (Clk cname, ty, ty') =>
  (print (BOLD_COLOR ^ RED_COLOR);
   print "### ERROR: Type error.\n";
   print ("  expects: " ^ (string_of_tag_t ty') ^ "\n");
   print ("  but got: " ^ (string_of_tag_t ty) ^ "\n");
   print ("  in: " ^ cname ^ "\n");
   print RESET_COLOR;
   OS.Process.exit OS.Process.failure
  )

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
	in if CalcParser.sameToken(nextToken,dummyEOF) then quit()
	  else loop lexer
      end
     in loop lexer
  end

(* Entry-point *)
val _ = (
  print ("Heron " ^ RELEASE_VERSION ^" Release\n");
  print "Type @help for assistance.\n";
  toplevel()
)
