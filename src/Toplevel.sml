(**
   Module Toplevel

   Author : Hai Nguyen Van
            LRI, Université Paris-Sud/CNRS
   
   The copyright to this code is held by Laboratoire de Recherche en
   Informatique, Université Paris-Sud/CNRS. All rights reserved. This
   file is distributed under the MIT License.
*)

(* Update this value for everytime code changes *)
val RELEASE_VERSION = "0.58.6-alpha+20181210"

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

(* Solver context variables *)
val maxstep                          = ref ~1
val minstep                          = ref ~1
val heuristics: TESL_atomic list ref = ref []
val dumpres                          = ref false
val rtprint                          = ref false
val file_to_open                     = ref ""

val declared_clocks: clock list ref = ref []
val declared_driving_clocks: clock list ref = ref []
val declared_clocks_quantities: clock list ref = ref [] (* included in declared_clocks *)

val snapshots: TESL_ARS_conf list ref = ref [([], 0, [], [])]
val current_step: int ref = ref 1

val clock_types: (clock * tag_t) list ref = ref []

fun quit () = case (!snapshots) of
    [] => OS.Process.exit OS.Process.failure
  | _  => OS.Process.exit OS.Process.success

fun action (stmt: TESL_atomic) =
  (* On-the-fly clock identifiers declaration *)
  let
    exception Toplevel_Action_Cannot_Happen
    val _ = declared_clocks := uniq ((!declared_clocks) @ (clocks_of_tesl_formula [stmt]))
    val _ = declared_clocks_quantities := uniq ((!declared_clocks_quantities) @ (quantities_of_tesl_formula [stmt]))
    val _ = clk_type_declare stmt clock_types
    val _ = type_check (!clock_types)
  in
  case stmt of
    DirMinstep n	     => minstep := n
  | DirMaxstep n	     => maxstep := n
  | DirHeuristic _	     => heuristics <>> stmt
  | DirDumpres	     => dumpres := true
  | DirScenario (strictness, step_index, tclks) =>
    let val scenario: system ref = ref []
        val n = (case step_index of NowPos  => if (!current_step) = 1 then 1 else (!current_step) - 1
				      | NextPos => !current_step
				      | Pos n   => n)
	 val () = writeln (BOLD_COLOR ^ MAGENTA_COLOR ^ "### Scenario [" ^ (string_of_int n) ^ "] ###" ^ RESET_COLOR)
	 val _ = List.app (fn (c, otag) =>
				 (scenario <>> (Ticks (c, n)) ;
				  case otag of NoneTag    => ()
					      | CstTag tag => scenario <>> (Timestamp (c, n, tag))
					      | SymbTag c_to_retrieve => (scenario <>> (Timestamp (c, n, Schematic (c, n))) ;
									      scenario <>> (AffineRefl (Schematic (c, n), Schematic (c_to_retrieve, n))))
			    )) tclks
	 val _ = if strictness
		  then List.app (fn c => if List.exists (fn (x, _) => x = c) tclks
					    then ()
					    else scenario <>> NotTicks (c, n)) (!declared_clocks)
		  else ()
	 val start_time = Time.now()
	 val () = snapshots := List.map (fn (G, n, phi, psi) => (G @ (!scenario), n, phi, psi)) (!snapshots)
	 val () = snapshots := List.map (fn (G, n, phi, psi) =>
						 let 
						   val G'   = (lfp reduce) G
						   val phi' = simplify_whentickings G' phi
						 in (G', n, phi', psi)
						 end) (!snapshots)
	 val () = snapshots := List.filter (fn (G, _, _, _) => SAT (!declared_clocks_quantities) G) (!snapshots)
	 val end_time = Time.now()
	 val _ = clear_line ()
	 val _ = writeln ("\r -> Consistent premodels: " ^ string_of_int (List.length (!snapshots)))
	 val _ = writeln (" -> Step solving time measured: " ^ Time.toString (Time.- (end_time, start_time)) ^ " s")
	 val _ = case (!snapshots) of
		      [] =>
		      (writeln (BOLD_COLOR ^ RED_COLOR ^ "### ERROR: No further state found.") ;
			writeln ("           Simulation is now stuck in inconsistent mode." ^ RESET_COLOR))
		    | _ => ()  
				 
    in ()
    end
  | DirDrivingClock c     => declared_driving_clocks <>>> c
  | DirEventConcretize index => snapshots := event_concretize (!declared_driving_clocks) (!clock_types) index (!snapshots)
  | DirRun stop_clks	     =>
      snapshots := exec
			  (!snapshots)
			  current_step
			  (!declared_clocks)
			  (!declared_clocks_quantities)
			  (!minstep, !maxstep, !dumpres, !heuristics, !rtprint, stop_clks)
  | DirRunStep	     =>
      snapshots := exec_step
			  (!snapshots)
			  current_step
			  (!declared_clocks)
			  (!declared_clocks_quantities)
			  (!minstep, !maxstep, !dumpres, !heuristics, !rtprint)
  | DirStutter	     =>
      snapshots := stutter_step
			  (!snapshots)
			  current_step
  | DirPrint selected_clocks => (case selected_clocks of
					  [] => print_dumpres (!declared_clocks) (!snapshots)
				      |  _  => print_dumpres selected_clocks (!snapshots))
  | DirOutputVCD          =>
    (case !snapshots of
	 []  => print (BOLD_COLOR ^ RED_COLOR ^ "## ERROR: No simulation state to write.\n" ^ RESET_COLOR)
      | [s] => 
	 (print ("## Writing vcd output to " ^ (OS.FileSys.getDir ()) ^ "/output.vcd\n");
	  writeFile "output.vcd" (VCD_toString RELEASE_VERSION (!current_step - 1) (!declared_clocks) s))
      | _   => print (BOLD_COLOR ^ RED_COLOR ^ "## ERROR: Too many states. Please do a selection first.\n" ^ RESET_COLOR))
  | DirOutputTEX (stdal, pdf, sel_clks)    =>
    (case !snapshots of
	 []  => print (BOLD_COLOR ^ RED_COLOR ^ "## ERROR: No simulation state to write.\n" ^ RESET_COLOR)
      | [s] =>
        let
	   val output_clks = if sel_clks = []
	   	     	then !declared_clocks
			else sel_clks
	 in
	  (print ("## Writing tex output to " ^ (OS.FileSys.getDir ()) ^ "/output.tex\n") ;
	   writeFile "output.tex" (TEX_toString stdal RELEASE_VERSION (!current_step - 1) (output_clks) s) ;
	   if pdf
	   then (
	     print ("## Calling pdflatex on " ^ (OS.FileSys.getDir ()) ^ "/output.tex\n") ;
	     let val return_st = OS.Process.system ("TEXINPUTS=$TEXINPUTS:\"./lib\" pdflatex -interaction=nonstopmode " ^ (OS.FileSys.getDir ()) ^ "/output.tex >/dev/null")
	     in if isSuccess return_st
		 then ()
		 else (print (BOLD_COLOR ^ RED_COLOR ^ "## ERROR: Failed at generating PDF output.\n          Check for pdflatex or TESL style file in ./lib/\n          > TEXINPUTS=$TEXINPUTS:\"./lib\" pdflatex " ^ (OS.FileSys.getDir ()) ^ "/output.tex\n" ^ RESET_COLOR) ;
			OS.Process.exit OS.Process.failure)
	     end
	   )
	   else ()
	  )
 	 end
      | _   => print (BOLD_COLOR ^ RED_COLOR ^ "## ERROR: Too many states. Please do a selection first.\n" ^ RESET_COLOR))
  | DirSelect n           =>
    (print ("## Selecting " ^ (Int.toString n) ^ "th simulation state\n");
     if n < 1 orelse n > List.length (!snapshots)
     then print (BOLD_COLOR ^ RED_COLOR ^ "## ERROR: State does not exist. Try again.\n" ^ RESET_COLOR)
     else snapshots := [List.nth (!snapshots, n - 1)]
     )
  | DirExit               => quit()
  | DirHelp               => print_help()
  | Precedes (c1, c2, _)  =>
    snapshots := List.map (fn (G, n, phi, psi) => (G, n, unsugar (!clock_types) (phi @ [stmt]), psi)) (!snapshots)
  | Excludes (c1, c2)  =>
    snapshots := List.map (fn (G, n, phi, psi) => (G, n, unsugar (!clock_types) (phi @ [stmt]), psi)) (!snapshots)
  | Kills (c1, c2)  =>
    snapshots := List.map (fn (G, n, phi, psi) => (G, n, unsugar (!clock_types) (phi @ [stmt]), psi)) (!snapshots)
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
		      let val _ = action stmt
              in TextIO.output(TextIO.stdOut, "val it = " ^ (string_of_expr stmt) ^ "\n")
              end
		     | NONE => ()
	in if CalcParser.sameToken(nextToken,dummyEOF) then quit()
	  else loop lexer
      end
     in loop lexer
  end

(* Reading from file *)
fun run_from_file s =
  let
    val dev = TextIO.openIn s
    val lexer = CalcParser.makeLexer (fn i => TextIO.inputN (dev, i))
    val dummyEOF = CalcLrVals.Tokens.EOF(0,0)
    val dummySEMI = CalcLrVals.Tokens.SEMI(0,0)
    fun loop lexer =
    let
	 val (result,lexer) = invoke lexer
	 val (nextToken,lexer) = CalcParser.Stream.get lexer
	 val _ = case result
		   of SOME stmt =>
		      let val _ = action stmt
              in ()
              end
		     | NONE => ()
	in if CalcParser.sameToken(nextToken,dummyEOF) then quit()
	  else loop lexer
      end
     in loop lexer
	 before TextIO.closeIn dev
  end

(* Entry-point *)
val _ = (
  print ("\u001B[1mHeron " ^ RELEASE_VERSION ^" Release\u001B[0m\n");
  let fun arg_loop args =
    case args of
        [] =>
     if (!file_to_open) = ""
     then
       (print "Copyright (c) 2018, A\195\169ropyr\195\169n\195\169es Flight Center, Universit\195\169 Paris-Sud / CNRS\n";
	    print "Type @help for assistance. Please cite:\n" ;
	    print "  H. Nguyen Van, T. Balabonski, F. Boulanger, C. Keller, B. Valiron, B. Wolff.\n";
	    print "  Formal Modeling and Analysis of Timed Systems (LNCS, volume 10419), pp 318-334\n";
	    toplevel())
     else
       (print ("## Opening " ^ (!file_to_open) ^ "\n") ;
        run_from_file (!file_to_open))
      | "-h" :: _ => print_help ()
      | "--help" :: _ => print_help ()
      | "--runtime-print" :: args' => (rtprint := true; arg_loop args')
      | "--use" :: filename :: args' => (file_to_open := filename; arg_loop args')
      | x :: _ =>
        (print ("Unknown option '" ^ x ^ "'.\n") ;
	  print_help () ;
	  OS.Process.exit OS.Process.failure)
    in arg_loop (CommandLine.arguments ())
    end
)

