(**
   Module ModelChecker

   Author : Hai Nguyen Van
            Université Paris-Saclay / CNRS
   
   The copyright to this code is held by Laboratoire de Recherche en
   Informatique, Université Paris-Sud/CNRS. All rights reserved. This
   file is distributed under the MIT License.
*)

val RELEASE_VERSION = "_VERSION_"

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

(* Model-checker state*)
type mc_params = {
  debug: bool ref,
  tesl_file: string ref,
  model: (TESL_conf AssocTree.t) ref, (* Identifier × Configuration *)
  lasso: UnionFind.t,
  CTL_formulae: (clock CTL.t) list ref,
  declared_clocks_CTL: clock list ref
}

(* Parameters for the model-checker *)
val mcp0: mc_params = {
  debug                   = ref false,
  tesl_file               = ref "",
  model                   = ref (AssocTree.Leaf (0, ([], 0, [], []))),
  lasso                   = UnionFind.make (),
  CTL_formulae            = ref [],
  declared_clocks_CTL     = ref []
}

fun print_debug s =
  if !(#debug mcp0)
  then print s
  else ()

(* Snapshots (also called configurations) *)
(* USELESS ? Just for initializing TESL formulae *)
val snapshots: solver_state =
  ref [([], 0, [], [])]

(* Immutable for model-checker *)
fun exec_step_immut
  (sp: solver_params)
  (cfs : TESL_conf list)
  : TESL_conf list =
  let
      (* ABORT SIMULATION IF NO REMAINING CONSISTENT SNAPSHOTS *)
      val () = case cfs of
		[] => raise Abort
	     | _  => ()
      val start_time = Time.now()

      (* 1. COMPUTING THE NEXT SIMULATION STEP *)
      (* DEBUG: Show the number of new branches (= of symbolic contexts) *)
      val _ = print_debug (BOLD_COLOR ^ BLUE_COLOR ^ "##### Solve: " ^ RESET_COLOR)
      (*  -- 1a. APPLYING INTRODUCTION RULES -- *)
      (*     (Γ, n ⊨ [] ▷ Φ) →i (_, _ ⊨ Φ ▷ Φ) *)
      val introduced_cfs = new_instant_init sp cfs
      (*  -- 1b. APPLYING ELIMINATION RULES UNTIL EMPTY PRESENT -- *)
      (*     (Γ, n ⊨ Ψ ▷ Φ) →e ... →e (_, _ ⊨ [] ▷ _) *)
      (*     NB. This is the heaviest part of the solver... Optimization is necessary. *)
      val reduce_psi_formulae = psi_reduce sp introduced_cfs
      (*  -- 1c. SIMPLIFYING Γ-CONTEXTS -- *)
      fun reduce_haa l = List.map (fn (G, n, phi, psi) =>
						    let 
						   (* val G'   = (lfp reduce) G *)
						      val G'   = (lfp (reduce_from (!(#current_step sp) - (pre_depth_formula phi)))) G
						      val phi' = simplify_whentickings G' phi
						    in (G', n, phi', psi)
						    end) l
      val reduced_haa_contexts = reduce_haa reduce_psi_formulae

      (* 2. REMOVE CONFIGURATIONS IN DEADLOCK STATE DUE TO UNMERGEABLE SPORADICS *)
      val cfs_no_deadlock = policy_no_spurious_sporadics sp (policy_no_spurious_whentickings sp reduced_haa_contexts)

      (* END OF SIMULATION *)
      val end_time = Time.now()
      (* val _ = #current_step sp := !(#current_step sp) + 1 *)
      val _ = print_debug ((string_of_int (List.length cfs_no_deadlock)) ^ " symbolic contexts / " ^ (Time.toString (Time.- (end_time, start_time))) ^ " s\n")
      val _ = case cfs_no_deadlock of
		    [] =>
		    (writeln (BOLD_COLOR ^ RED_COLOR ^ "### ERROR: No further state found.") ;
		     writeln ("           Simulation is now stuck in inconsistent mode." ^ RESET_COLOR))
		  | _ => ()  
  in cfs_no_deadlock
  end
  handle
    Abort => (print_dumpres (!(#declared_clocks sp)) []; [])

(* BETA: Only considers Ticks and NotTicks. *)
fun concretize_contexts
  (mcp: mc_params)
  (cfs : TESL_conf list)
  : TESL_conf list =
  let fun concretize_context_wrt_clock ((G,n,phi,psi): TESL_conf) (c: clock): TESL_conf list =
	   if (List.exists (fn Ticks(c',n') => c=c' andalso n=n' | _ => false) G)
	      orelse (List.exists (fn NotTicks(c',n') => c=c' andalso n=n' | _ => false) G)
	   then [(G,n,phi,psi)]
	   else [(Ticks(c,n) :: G,n,phi,psi), (NotTicks(c,n) :: G,n,phi,psi)]
      fun concretize_contexts_wrt_clocks (cfs : TESL_conf list) (clks: clock list) =
    case clks of
      []         => cfs
    | c :: clks' => concretize_contexts_wrt_clocks (List.concat (List.map (fn cf => concretize_context_wrt_clock cf c) cfs)) clks'
      val res = concretize_contexts_wrt_clocks cfs (!(#declared_clocks_CTL mcp))
      val _   = print_debug ("          as " ^ (Int.toString (List.length res)) ^ " concrete contexts\n")
  in res
  end

fun quit () = case (!snapshots) of
    [] => OS.Process.exit OS.Process.failure
  | _  => OS.Process.exit OS.Process.success

fun tesl_action (stmt: TESL_atomic) =
  (* On-the-fly clock identifiers declaration *)
  let
    exception Toplevel_Action_Cannot_Happen
    val _ = #declared_clocks sp0 := uniq (!(#declared_clocks sp0) @ (clocks_of_tesl_formula [stmt]))
    val _ = #declared_quantities sp0 := uniq (!(#declared_quantities sp0) @ (quantities_of_tesl_formula [stmt]))
    val _ = case stmt of
      DirCTLFormula f => (#declared_clocks_CTL mcp0) := ((!(#declared_clocks_CTL mcp0)) @\/ (CTL.clocks_of_CTL f))
      | _             => ()
    val _ = clk_type_declare sp0 stmt
    val _ = type_check sp0
  in
  case stmt of
    DirMinstep n	                      => ()
  | DirMaxstep n	                      => ()
  | DirHeuristic _	                      => ()
  | DirDumpres	                      => ()
  | DirScenario sc_params                  => ()
  | DirDrivingClock c                      => ()
  | DirEventConcretize index               => ()
  | DirRun stop_clks	                      => ()
  | DirRunStep	                      => ()
  | DirStutter	                      => ()
  | DirPrint selected_clocks               => ()
  | DirOutputVCD                           => ()
  | DirOutputCSV (sel_clks, filename)      => ()
  | DirOutputTEX (stdal, pdf, sel_clks)    => ()
  | DirSelect sel                          => ()
  | DirUniq                                => ()
  | DirExit                                => quit()
  | DirHelp                                => ()
  | DirCTLFormula (f)                      => (#CTL_formulae mcp0) := (!(#CTL_formulae mcp0)) @ [f]
  | _                                      =>
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

(* Reading from file *)
fun tesl_from_file s =
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
		      let val _ = tesl_action stmt
              in ()
              end
		     | NONE => ()
	in if CalcParser.sameToken(nextToken,dummyEOF) then () (* quit() *)
	  else loop lexer
      end
     in loop lexer
	 before TextIO.closeIn dev
  end

fun print_help () = let
  val _ = print "\n"
  val _ = print "Usage: hmc --use [TESL FILE]\n"
  val _ = print "Decides if a TESL model satisfies an CTL formula.\n"
in ()
end

fun abstract (F: TESL_formula) =
  List.map (fn TimeDelayedBy (master, _, meas, reset, slave) => TimeDelayedBy_Abs (master, meas, reset, slave)
	      | SporadicOn (slave, _, meas) => SporadicOn_Abs (slave, meas)
             | f => f) F

fun lasso_equiv (c1: TESL_conf) (c2: TESL_conf): bool =
    let val (_, _, phi1, _) = c1
	 val (_, _, phi2, _) = c2
    in ListMore.equals phi1 phi2
    end

(* Yields the successors state identifiers of a state identifier *)
fun next (ids: int list): int list=
  let fun next_id (id: int): int list =
    let val representative_id = UnionFind.find (#lasso mcp0) id
	 val assoc_subtrees    = case AssocTree.sub representative_id (!(#model mcp0)) of
          AssocTree.Leaf _             => []
        | AssocTree.Node (_, subtrees) => subtrees
	 val as_ids            = List.map (fn AssocTree.Leaf (n,_)     => n
                                           | AssocTree.Node ((n,_),_) => n) assoc_subtrees
    in as_ids
    end
  in uniq (List.concat (List.map (next_id) ids))
  end

exception BadInitialization
fun model_maker () =
let 
  val _ = print (BOLD_COLOR ^ CYAN_COLOR ^ "## Generation of the abstract model\n" ^ RESET_COLOR)
  (* Initialize the TESL model *)
  val _ = (#model mcp0) := (AssocTree.Leaf (0, List.nth (!snapshots, 0)))
  val _ = (#model mcp0) := (case (!(#model mcp0)) of
				   AssocTree.Leaf (0, (G, n, phi, psi)) => AssocTree.Leaf (0, (G, n, abstract phi, psi))
				 | _ => raise BadInitialization)
  fun loop () = let    
    (* val _ = AssocTree.info (!(#model mcp0)) *)
    val leaves = AssocTree.leaves (!(#model mcp0))
    val leaves_to_grow = List.filter (fn (n, cf) => n = UnionFind.find (#lasso mcp0) n) leaves
    (* Grow leaves one more step*)
    val _ = print (BOLD_COLOR ^ CYAN_COLOR ^ "###### Depth [" ^ (Int.toString (!(#current_step sp0))) ^ "] ######\n" ^ RESET_COLOR)
    (* Unfold at each leaf *)
    (* Run contexts only contains primitives for current instant*)
    val iterated_leaves =
	 List.map (fn (n, cf) => (n,
				     List.map (fn (G, n, phi, psi) =>
						    let val G_current = fst (List.partition (ContextSplit.indx_geq n) G)
						    in (G_current, n, phi, psi)
						    end) (concretize_contexts mcp0 (exec_step_immut sp0 [cf])))) leaves_to_grow
    (* Replace leaves by nodes in computation tree *)
    val _ = List.foldl
     		(fn ((n, cfs), _) => (#model mcp0) := AssocTree.grow (!(#model mcp0)) n cfs)
     		()
     		iterated_leaves
    val _ = #current_step sp0 := !(#current_step sp0) + 1

    (* Finding lassos *)
    val new_leaves = AssocTree.leaves (!(#model mcp0))
    val new_nodes = AssocTree.nodes (!(#model mcp0))
    val _ = List.foldl
		  (fn ((n, cf), _) =>
		      List.foldl
			   (fn ((n', cf'), _) => 
				if lasso_equiv cf cf'
				then (UnionFind.union (#lasso mcp0) n' n (* ;
				      print (" <- UNIFIED " ^ (Int.toString n) ^ " WITH " ^ (Int.toString n') ^ "\n") ;
				      UnionFind.print (#lasso mcp0) 10 *) )
				else ()
			   ) () new_nodes
		  )
		  ()
		  new_leaves
(*
    val _ = UnionFind.print (#lasso mcp0) 10
*)
    (* Leaves pending for refold *)
    val leaves_to_refold = List.filter (fn (n, cf) => n = UnionFind.find (#lasso mcp0) n) new_leaves
    val _ = case List.length leaves_to_refold of
		  0 => (print ("## Abstract model: " ^ BOLD_COLOR ^ GREEN_COLOR ^ "COMPLETE.\n" ^ RESET_COLOR) ;
			 print ("   No. of nodes:  " ^ (Int.toString (AssocTree.count_node (!(#model mcp0)))) ^ "\n") ;
			 print ("   No. of leaves: " ^ (Int.toString (AssocTree.count_leaf (!(#model mcp0)))) ^ "\n")
			)
		| n => (print ("## Abstract model: " ^ BOLD_COLOR ^ YELLOW_COLOR ^ (Int.toString n) ^ " branches to refold.\n" ^ RESET_COLOR);
			 if !(#debug mcp0)
			 then List.foldl 
			   (fn ((_,(G,n,phi,psi)), _) => (print "\206\166 = { " ; (List.app (fn f => print (BOLD_COLOR ^ YELLOW_COLOR ^ (string_of_expr f) ^ " ; " ^ RESET_COLOR)) phi) ; print " }\n")) 
			   ()
			   leaves_to_refold
			   else () ;
			 loop ())
  in ()
  end
in loop ()
end

fun semantics_pre_all (T: int list): int list =
  let val states = AssocTree.id_list (!(#model mcp0)) (* can be optimized... *)
  in List.filter (fn s => List.all (fn s' => contains s' T) (next [s])) states
  end

fun semantics_pre_exists (T: int list): int list = 
  let val states = AssocTree.id_list (!(#model mcp0)) (* can be optimized... *)
  in List.filter (fn s => List.exists (fn s' => contains s' T) (next [s])) states
  end

fun semantics_CTL (f: clock CTL.t): int list =
  let val states = AssocTree.id_list (!(#model mcp0)) (* can be optimized... *)
  in case f of
    CTL.True             => states
  | CTL.False            => []
  | CTL.Atom c           => (* Completeness by not using NotTicks is ensured by concretized runs *)
      List.filter (fn state_id => let val (G,n,_,_) = AssocTree.assoc state_id (!(#model mcp0))
				in List.exists (fn Ticks (c',n') => c = c' andalso n = n' | _ => false) G
				end) states
  | CTL.Not f'           => states @- (semantics_CTL f')
  | CTL.And (f1, f2)     => (semantics_CTL f1) @/\ (semantics_CTL f2)
  | CTL.Or (f1, f2)      => (semantics_CTL f1) @\/ (semantics_CTL f2)
  | CTL.Implies (f1, f2) => semantics_CTL (CTL.Or (CTL.Not f1, f2))
  | CTL.Iff (f1, f2)     => semantics_CTL (CTL.And (CTL.Implies(f1, f2), CTL.Implies(f2, f1)))
  | CTL.AX f'            =>	semantics_pre_all (semantics_CTL f')
  | CTL.EX f'            =>	semantics_pre_exists (semantics_CTL f')
  | CTL.AF f'            =>	lfp_set (fn X => X @\/ (semantics_pre_exists X)) (semantics_CTL f')
  | CTL.EF f'            =>	lfp_set (fn X => X @\/ (semantics_pre_all X)) (semantics_CTL f')
  | CTL.AG f'            =>	semantics_CTL (CTL.Not (CTL.EF (CTL.Not f')))
  | CTL.EG f'            =>	semantics_CTL (CTL.Not (CTL.AF (CTL.Not f')))
  | CTL.AU (f1, f2)      =>	lfp_set (fn X => X @\/ ((semantics_CTL f1) @/\ (semantics_pre_all X))) (semantics_CTL f2)
  | CTL.EU (f1, f2)      =>	lfp_set (fn X => X @\/ ((semantics_CTL f1) @/\ (semantics_pre_exists X))) (semantics_CTL f2)
    end

exception Undefined_SAT_CTL_Procedure
(* Main SAT decision procedure for LTL formulae *)
fun SAT_CTL (state_id: int) (f: clock CTL.t): bool =
  contains state_id (semantics_CTL f)
(*
fun SAT_CTL (state_id: int) (f: clock CTL.t): bool = case f of 
    CTL.True             => true
  | CTL.False            => false
  | CTL.Atom c           => (* Completeness by not using NotTicks is ensured by concretized runs *)
      let val (G,n,_,_) = AssocTree.assoc state_id (!(#model mcp0))
      in List.exists (fn Ticks (c',n') => c = c' andalso n = n' | _ => false) G
      end
  | CTL.Not f'           => not (SAT_CTL state_id f')
  | CTL.And (f1, f2)     => (SAT_CTL state_id f1) andalso (SAT_CTL state_id f2)
  | CTL.Or (f1, f2)      => (SAT_CTL state_id f1) orelse (SAT_CTL state_id f2)
  | CTL.Implies (f1, f2) => (not (SAT_CTL state_id f1)) orelse (SAT_CTL state_id f2)
  | CTL.Iff (f1, f2)     => (SAT_CTL state_id f1) = (SAT_CTL state_id f2)
  | CTL.AX f'            =>
      let val next_states = next [state_id]
      in List.all (fn id => SAT_CTL id f') next_states
      end
  | CTL.EX f'            =>
      let val next_states = next [state_id]
      in List.exists (fn id => SAT_CTL id f') next_states
      end
  | CTL.EF f'            =>
      let val next_states = lfp_set (next) [state_id]
      in List.exists (fn id => SAT_CTL id f') next_states
      end
  | CTL.AG f'            =>
      let val next_states = lfp_set (next) [state_id]
      in List.all (fn id => SAT_CTL id f') next_states
      end
  | _                    => raise Undefined_SAT_CTL_Procedure
*)

(* Entry-point *)
val _ = (
  print ("\u001B[1mHeron Model Checker (part of " ^ RELEASE_VERSION ^ " Release)\u001B[0m\n");
  let fun arg_loop args =
    case args of
        [] =>
     if (!(#tesl_file mcp0)) = ""
     then
       (print "Copyright (c) 2020, Universit\195\169 Paris-Saclay / CNRS\n";
	print_help ())
	    
     else
	let
	  val _ = print ("## Opening " ^ (!(#tesl_file mcp0)) ^ "\n")
	  val _ = tesl_from_file (!(#tesl_file mcp0))
	  val _ = print "## Declared clocks and flows in TESL: "
	  val _ = (List.app (fn Clk cname => print (cname ^ ", ")) (!(#declared_clocks sp0)); print "\n")
	  val _ = print "## Declared clocks and flows in CTL: "
	  val _ = (List.app (fn Clk cname => print (cname ^ ", ")) (!(#declared_clocks_CTL mcp0)); print "\n")
	  val _ = model_maker ()
	  (* DEBUG: To print successors states *)
	  (*
	  val test0 = next [23](* lfp_set (next) [0] *)
	  val _ = (print "[" ; List.app (fn n => print ((Int.toString n) ^ " ")) test0 ; print "]\n")
	  *)
	  (* ********************************* *)
	  val _ = print "## Properties:\n"
	  val _ = List.app (fn f => (
				 print ("[         ] " ^ (CTL.toString f)) ;
				 if SAT_CTL 0 f
				 then print ("\r" ^ BOLD_COLOR ^ GREEN_COLOR ^ "[  VALID  ] " ^ RESET_COLOR ^ (CTL.toString f) ^ "\n")
				 else print ("\r" ^ BOLD_COLOR ^ RED_COLOR   ^ "[ UNKNOWN ] " ^ RESET_COLOR ^ (CTL.toString f) ^ "\n") ;
				 (print_debug "            \226\138\163 [" ; List.app (fn n => print_debug ((Int.toString n) ^ " ")) (semantics_CTL f) ; print_debug "]\n")
			     )) (!(#CTL_formulae mcp0))
	in ()
	end
      | "-h" :: _ => print_help ()
      | "--help" :: _ => print_help ()
      | "--debug" :: args' => (#debug mcp0 := true ; arg_loop args')
      | "--use" :: filename :: args' => (#tesl_file mcp0 := filename; arg_loop args')
      | x :: _ =>
        (print ("Unknown option '" ^ x ^ "'.\n") ;
	  print_help () ;
	  OS.Process.exit OS.Process.failure)
    in arg_loop (CommandLine.arguments ())
    end
)

