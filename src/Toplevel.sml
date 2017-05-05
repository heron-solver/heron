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
print (BOLD_COLOR ^ "Simulation Solver for Timed Causality Models in the Tagged Events Specification Language\n" ^ RESET_COLOR); 
print "\n";
print "Copyright (c) 2017, Hai Nguyen Van\n";
print "              Universit\195\169 Paris-Sud / CNRS\n";
(*
print "Please cite:";
print "\n";
*)
print "\n";
print (BOLD_COLOR ^ "TESL language expressions:\n" ^ RESET_COLOR); 
print "  [CLOCK] \u001B[1msporadic\u001B[0m [TAG]+\n"; 
print "  [CLOCK] \u001B[1mperiodic\u001B[0m [TAG] (\u001B[1moffset\u001B[0m [TAG])\n"; 
print "  [CLOCK] \u001B[1mimplies\u001B[0m [CLOCK]\n"; 
print "  \u001B[1mtag relation\u001B[0m [CLOCK] = [TAG] * [CLOCK] + [TAG]\n"; 
print "  [CLOCK] \u001B[1mtime delayed by\u001B[0m [TAG] \u001B[1mon\u001B[0m [CLOCK] \u001B[1mimplies\u001B[0m [CLOCK]\n"; 
print "  [CLOCK] \u001B[1mdelayed by\u001B[0m [INT] \u001B[1mon\u001B[0m [CLOCK] \u001B[1mimplies\u001B[0m [CLOCK]\n"; 
print "  [CLOCK] \u001B[1mfiltered by\u001B[0m [INT], [INT] ([INT], [INT])* \u001B[1mimplies\u001B[0m [CLOCK]\n"; 
print "  [CLOCK] \u001B[1mevery\u001B[0m [INT] \u001B[1mimplies\u001B[0m [CLOCK]\n";
print "  [CLOCK] \u001B[1msustained\u001B[0m (\u001B[1mimmediately\u001B[0m) \u001B[1mfrom\u001B[0m [CLOCK] \u001B[1mto\u001B[0m [CLOCK] (\u001B[1mweakly\u001B[0m) \u001B[1mimplies\u001B[0m [CLOCK]\n"; 
print "  [CLOCK] \u001B[1mnext to\u001B[0m [CLOCK] \u001B[1mimplies\u001B[0m [CLOCK]\n"; 
print "  \u001B[1mawait\u001B[0m [CLOCK]+ \u001B[1mimplies\u001B[0m [CLOCK]\n"; 
print "  [CLOCK] \u001B[1mwhen\u001B[0m (\u001B[1mnot\u001B[0m) [CLOCK] \u001B[1mimplies\u001B[0m [CLOCK]\n"; 
print "\n"; 
print "  For more information about the TESL language:\n"; 
print "  http://wwwdi.supelec.fr/software/TESL\n"; 
print "\n"; 
print (BOLD_COLOR ^ "Run parameters:\n" ^ RESET_COLOR);  
print "  @minstep [INT]                    define the number of minimum run steps\n"; 
print "  @maxstep [INT]                    define the number of maximum run steps\n"; 
print "  @policy [NAME]                    load a simulation policy among:\n";
print "                                      \u001B[1masap\u001B[0m\n"; 
print "                                      \u001B[1mminimize_ticks\u001B[0m\n";
print "                                      \u001B[1mspeedup_event_occ\u001B[0m\n";
print "                                      \u001B[1mminimize_floating_ticks\u001B[0m\n";
print "                                      \u001B[1mminimize_unsolved_affine\u001B[0m\n";
print "                                      \u001B[1mno_empty_instants\u001B[0m\n";
print "                                      \u001B[1mmaximize_reactiveness\u001B[0m\n";
print "  @dumpres                          option to display the results after @run\n"; 
print "  @scenario (strict) [INT] [CLOCK]+ refine snapshots with instantaneous scenario\n"; 
print "  @scenario (strict) next [CLOCK]+  refine snapshots of next simulation step\n"; 
print "  @select [INT]                     select by keeping only one simulation state\n"; 
print "\n"; 
print (BOLD_COLOR ^ "Interactive commands:\n" ^ RESET_COLOR);  
print "  @exit                          exit Heron\n"; 
print "  @run                           run the specification until model found\n"; 
print "  @step                          run the specification for one step\n"; 
print "  @print                         display the current snapshots\n"; 
print "  @output vcd                    export to VCD file\n"; 
print "  @help                          display the list of commands\n")

val maxstep                          = ref ~1
val minstep                          = ref ~1
val heuristics: TESL_atomic list ref = ref []
val dumpres                          = ref false

val scenario: system ref = ref []

val declared_clocks: clock list ref = ref []
val declared_driving_clocks: clock list ref = ref []

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
  | DirHeuristic _	     => heuristics <>> stmt
  | DirDumpres	     => dumpres := true
  | DirScenario (strictness, step_index, tclks) =>
    let val n = (case step_index of NONE => !current_step
				      | SOME n => n)
	 val _ = List.app (fn (c, otag) =>
				 (scenario <>> (Ticks (c, n)) ;
				  case otag of SOME tag => scenario <>> (Timestamp (c, n, tag)) | NONE => ())) tclks
	 val _ = if strictness
		  then List.app (fn c => if List.exists (fn (x, _) => x = c) tclks
					    then ()
					    else scenario <>> NotTicks (c, n)) (!declared_clocks)
		  else ()
    in ()
    end
  | DirDrivingClock c     => declared_driving_clocks <>>> c
  | DirRun		     =>
      snapshots := exec
			  (!snapshots)
			  current_step
			  (!declared_clocks)
			  (!minstep, !maxstep, !dumpres, !scenario, !heuristics)
  | DirRunStep	     =>
      snapshots := exec_step
			  (!snapshots)
			  current_step
			  (!declared_clocks)
			  (!minstep, !maxstep, !dumpres, !scenario, !heuristics)
  | DirPrint              => print_dumpres (!declared_clocks) (!snapshots)
  | DirOutputVCD          =>
    (case !snapshots of
	 []  => print (BOLD_COLOR ^ RED_COLOR ^ "## ERROR: No simulation state to write.\n" ^ RESET_COLOR)
      | [s] => 
	 (print ("## Writing vcd output to " ^ (OS.FileSys.getDir ()) ^ "/output.vcd\n");
	  writeFile "output.vcd" (VCD_toString (!current_step - 1) (!declared_clocks) s))
      | _   => print (BOLD_COLOR ^ RED_COLOR ^ "## ERROR: Too many states. Please do a selection first.\n" ^ RESET_COLOR))
  | DirSelect n           =>
    (print ("## Selecting " ^ (Int.toString n) ^ "th simulation state\n");
     if n < 1 orelse n > List.length (!snapshots)
     then print (BOLD_COLOR ^ RED_COLOR ^ "## ERROR: State does not exist. Try again.\n" ^ RESET_COLOR)
     else snapshots := [List.nth (!snapshots, n - 1)]
     )
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
