(**
   Module Toplevel

   Author : Hai Nguyen Van
            Université Paris-Saclay / CNRS
   
   The copyright to this code is held by Laboratoire de Recherche en
   Informatique, Université Paris-Sud/CNRS. All rights reserved. This
   file is distributed under the MIT License.
*)

(* Update this value for everytime code changes *)
val RELEASE_VERSION = "0.62.1-alpha+20200326"
val COMPILER_CMD = "_COMPILER_CMD_"

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

(* Variables specific to the solver *)
(* Parameters *)
val sp0: solver_params = {
  maxstep                 = ref ~1,
  minstep                 = ref ~1,
  heuristics              = ref [],
  dumpres                 = ref false,
  rtprint                 = ref false,
  file_to_open            = ref "",
  declared_clocks         = ref [],
  declared_quantities     = ref [], (* included in declared_clocks *)
  declared_clocks_driving = ref [],
  clock_types             = ref [],
  current_step            = ref 1
}
(* Snapshots (also called configurations) *)
val snapshots: solver_state =
  ref [([], 0, [], [])]

fun quit () = case (!snapshots) of
    [] => OS.Process.exit OS.Process.failure
  | _  => OS.Process.exit OS.Process.success

fun action (stmt: TESL_atomic) =
  (* On-the-fly clock identifiers declaration *)
  let
    exception Toplevel_Action_Cannot_Happen
    val _ = #declared_clocks sp0 := uniq (!(#declared_clocks sp0) @ (clocks_of_tesl_formula [stmt]))
    val _ = #declared_quantities sp0 := uniq (!(#declared_quantities sp0) @ (quantities_of_tesl_formula [stmt]))
    val _ = clk_type_declare sp0 stmt
    val _ = type_check sp0
  in
  case stmt of
    DirMinstep n	     => #minstep sp0 := n
  | DirMaxstep n	     => #maxstep sp0 := n
  | DirHeuristic _	     => (#heuristics sp0) <>> stmt
  | DirDumpres	     => #dumpres sp0 := true
  | DirScenario sc_params => 
      scenario_add sp0 sc_params snapshots
  | DirDrivingClock c     => (#declared_clocks_driving sp0) <>>> c
  | DirEventConcretize index =>
      snapshots := event_concretize sp0 index (!snapshots)
  | DirRun stop_clks	     =>
      snapshots := exec sp0 stop_clks (!snapshots)
  | DirRunStep	     =>
      snapshots := exec_step sp0 (!snapshots)
  | DirStutter	     =>
      snapshots := stutter_step sp0 (!snapshots)
  | DirPrint selected_clocks => (case selected_clocks of
					  [] => print_dumpres (!(#declared_clocks sp0)) (!snapshots)
				      |  _  => print_dumpres selected_clocks (!snapshots))
  | DirOutputVCD          =>
    (case !snapshots of
	 []  => print (BOLD_COLOR ^ RED_COLOR ^ "## ERROR: No simulation state to write.\n" ^ RESET_COLOR)
      | [s] => 
	 (print ("## Writing vcd output to " ^ (OS.FileSys.getDir ()) ^ "/output.vcd\n");
	  writeFile "output.vcd" (VCD_toString RELEASE_VERSION (!(#current_step sp0) - 1) (!(#declared_clocks sp0)) s))
      | _   => print (BOLD_COLOR ^ RED_COLOR ^ "## ERROR: Too many states. Please do a selection first.\n" ^ RESET_COLOR))
  | DirOutputTEX (stdal, pdf, sel_clks)    =>
    (case !snapshots of
	 []  => print (BOLD_COLOR ^ RED_COLOR ^ "## ERROR: No simulation state to write.\n" ^ RESET_COLOR)
      | [s] =>
        let
	   val output_clks = if sel_clks = []
	   	     	then (!(#declared_clocks sp0))
			else sel_clks
	 in
	  (print ("## Writing tex output to " ^ (OS.FileSys.getDir ()) ^ "/output.tex\n") ;
	   writeFile "output.tex" (TEX_toString stdal RELEASE_VERSION (!(#current_step sp0) - 1) (output_clks) s) ;
	   if pdf
	   then (
	     print ("## Calling pdflatex on " ^ (OS.FileSys.getDir ()) ^ "/output.tex\n") ;
	     let val return_st = OS.Process.system ("TEXINPUTS=$TEXINPUTS:\"./lib\" pdflatex -interaction=nonstopmode \"" ^ (OS.FileSys.getDir ()) ^ "/output.tex\" >/dev/null")
	     in if isSuccess return_st
		 then ()
		 else (print (BOLD_COLOR ^ RED_COLOR ^ "## ERROR: Failed at generating PDF output.\n          Check for pdflatex or TESL style file in ./lib/\n          > TEXINPUTS=$TEXINPUTS:\"./lib\" pdflatex \"" ^ (OS.FileSys.getDir ()) ^ "/output.tex\"\n" ^ RESET_COLOR) ;
			OS.Process.exit OS.Process.failure)
	     end
	   )
	   else ()
	  )
 	 end
      | _   => print (BOLD_COLOR ^ RED_COLOR ^ "## ERROR: Too many states. Please do a selection first.\n" ^ RESET_COLOR))
  | DirSelect sel           => (case sel of
      Ordinal n =>
      (print ("## Selecting simulation state #" ^ (Int.toString n) ^ "\n");
       if n < 1 orelse n > List.length (!snapshots)
       then print (BOLD_COLOR ^ RED_COLOR ^ "## ERROR: State does not exist. Try again.\n" ^ RESET_COLOR)
       else snapshots := [List.nth (!snapshots, n - 1)]
      )
    | Hexadecimal n =>
      let (* val _ = if Hash.find_collision (!snapshots)
	     then (writeln (BOLD_COLOR ^ YELLOW_COLOR ^ "### WARNING: An hash collision exists." ^ RESET_COLOR);
		    writeln (BOLD_COLOR ^ YELLOW_COLOR ^ "             Selecting collided hashes may be inconsistent." ^ RESET_COLOR))
	     else () *)
	   val _ = print ("## Selecting simulation state 0x" ^ (Int.fmt StringCvt.HEX n) ^ "\n")
	   val picked = List.find (fn cf => (Hash.str_hash_of_TESL_conf cf) = (Int.fmt StringCvt.HEX n)) (!snapshots)
	in case picked of
	  NONE    => print (BOLD_COLOR ^ RED_COLOR ^ "## ERROR: State does not exist. Try again.\n" ^ RESET_COLOR)
	| SOME cf =>
	  if Hash.is_forbidden (!snapshots) n
	  then print (BOLD_COLOR ^ RED_COLOR ^ "## ERROR: Forbidden hash due to collision.\n" ^ RESET_COLOR)
	  else snapshots := [cf]
	end
  )
  | DirExit               => quit()
  | DirHelp               => print_help()
  | _                     =>
    snapshots := List.map (fn (G, n, phi, psi) => (G, n, unsugar (!(#clock_types sp0)) (phi @ [stmt]), psi)) (!snapshots)
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
  print ("\u001B[1mHeron " ^ RELEASE_VERSION ^ " [" ^ COMPILER_CMD ^  "] Release\u001B[0m\n");
  let fun arg_loop args =
    case args of
        [] =>
     if !(#file_to_open sp0) = ""
     then
       (print "Copyright (c) 2020, Universit\195\169 Paris-Saclay / CNRS\n";
	    print "Type @help for assistance. Please cite:\n" ;
	    print "  H. Nguyen Van, T. Balabonski, F. Boulanger, C. Keller, B. Valiron, B. Wolff.\n";
	    print "  Formal Modeling and Analysis of Timed Systems (LNCS, volume 10419), pp 318-334\n";
	    toplevel())
     else
       (print ("## Opening " ^ (!(#file_to_open sp0)) ^ "\n") ;
        run_from_file (!(#file_to_open sp0)))
      | "-h" :: _ => print_help ()
      | "--help" :: _ => print_help ()
      | "--runtime-print" :: args' => (#rtprint sp0 := true; arg_loop args')
      | "--use" :: filename :: args' => (#file_to_open sp0 := filename; arg_loop args')
      | x :: _ =>
        (print ("Unknown option '" ^ x ^ "'.\n") ;
	  print_help () ;
	  OS.Process.exit OS.Process.failure)
    in arg_loop (CommandLine.arguments ())
    end
)

