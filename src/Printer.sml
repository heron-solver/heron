(**
   Module Printer

   Author : Hai Nguyen Van
            LRI, Université Paris-Sud/CNRS
   
   The copyright to this code is held by Laboratoire de Recherche en
   Informatique, Université Paris-Sud/CNRS. All rights reserved. This
   file is distributed under the MIT License.
*)

fun print_help () = (
print (BOLD_COLOR ^ "Heron\n" ^ RESET_COLOR); 
print (BOLD_COLOR ^ "Simulation Solver for Timed Causality Models in the Tagged Events Specification Language\n" ^ RESET_COLOR); 
print ("Usage: " ^ CommandLine.name () ^ " [--use FILE.tesl] [--runtime-print]");
print "\n\n";
print "Copyright (c) 2018, A\195\169ropyr\195\169n\195\169es Flight Center, Universit\195\169 Paris-Sud / CNRS\n";
print "Please cite: H. Nguyen Van, T. Balabonski, F. Boulanger, C. Keller, B. Valiron, B. Wolff.\n";
print "             Formal Modeling and Analysis of Timed Systems (LNCS, volume 10419), pp 318-334.\n";
print "\n";
print (BOLD_COLOR ^ "TESL language expressions:\n" ^ RESET_COLOR); 
print "  [CLOCK] \u001B[1msporadic\u001B[0m [TAG]+\n"; 
print "  [CLOCK] \u001B[1msporadic\u001B[0m [TAG] \u001B[1mon\u001B[0m [CLOCK]\n"; 
print "  [CLOCK] \u001B[1mperiodic\u001B[0m [TAG] (\u001B[1moffset\u001B[0m [TAG])\n"; 
print "  [CLOCK] \u001B[1mimplies (not)\u001B[0m [CLOCK]\n"; 
print "  \u001B[1mtime relation\u001B[0m [CLOCK] = [TAG] * [CLOCK] + [TAG]\n"; 
print "  [CLOCK] \u001B[1mtime delayed by\u001B[0m [TAG] \u001B[1mon\u001B[0m [CLOCK] (\u001B[1mwith reset on\u001B[0m [CLOCK]) \u001B[1mimplies\u001B[0m [CLOCK]\n";
print "  [CLOCK] \u001B[1mdelayed by\u001B[0m [INT] \u001B[1mon\u001B[0m [CLOCK] \u001B[1mimplies\u001B[0m [CLOCK]\n"; 
print "  [CLOCK] \u001B[1mfiltered by\u001B[0m [INT], [INT] ([INT], [INT])* \u001B[1mimplies\u001B[0m [CLOCK]\n"; 
print "  [CLOCK] \u001B[1mevery\u001B[0m [INT] \u001B[1mimplies\u001B[0m [CLOCK]\n";
print "  [CLOCK] \u001B[1msustained\u001B[0m (\u001B[1mimmediately\u001B[0m) \u001B[1mfrom\u001B[0m [CLOCK] \u001B[1mto\u001B[0m [CLOCK] (\u001B[1mweakly\u001B[0m) \u001B[1mimplies\u001B[0m [CLOCK]\n"; 
print "  [CLOCK] \u001B[1mnext to\u001B[0m [CLOCK] \u001B[1mimplies\u001B[0m [CLOCK]\n"; 
print "  \u001B[1mawait\u001B[0m [CLOCK]+ \u001B[1mimplies\u001B[0m [CLOCK]\n"; 
print "  [CLOCK] \u001B[1mwhen\u001B[0m (\u001B[1mnot\u001B[0m) [CLOCK] \u001B[1mimplies\u001B[0m [CLOCK]\n"; 
print "\n"; 
print "  For more information about the TESL language:\n"; 
print "  http://wdi.supelec.fr/software/TESL/\n"; 
print "\n";
print (BOLD_COLOR ^ "Extensions in CCSL-style:\n" ^ RESET_COLOR); 
print "  [CLOCK] \u001B[1m[strictly | weakly] precedes\u001B[0m [CLOCK]\n"; 
print "  [CLOCK] \u001B[1mexcludes\u001B[0m [CLOCK]\n"; 
print "  [CLOCK] \u001B[1mkills\u001B[0m [CLOCK]\n"; 
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
print "  @driving-clock [CLOCK]+           declare driving clocks\n"; 
print "\n"; 
print (BOLD_COLOR ^ "Interactive commands:\n" ^ RESET_COLOR);  
print "  @exit                             exit Heron\n"; 
print "  @run                              run the specification until model found\n"; 
print "  @step                             run the specification for one step\n"; 
print "  @event-concretize                 concretize ticks/tags of driving clocks\n"; 
print "  @print                            display the current snapshots\n"; 
print "  @output vcd/tikz/tex/pdf [CLOCK]* export to VCD/TikZ/LaTeX/PDF file with clock selection\n"; 
print "  @help                             display the list of commands\n")

fun superscript_of_char (c : char) =
  case c of
      #"a" => "\225\181\131"
    | #"b" => "\225\181\135"
    | #"c" => "\225\182\156"
    | #"d" => "\225\181\136"
    | #"e" => "\225\181\137"
    | #"f" => "\225\182\160"
    | #"g" => "\225\181\141"
    | #"h" => "\202\176"
    | #"i" => "\226\129\177"
    | #"j" => "\202\178"
    | #"k" => "\225\181\143"
    | #"l" => "\203\161"
    | #"m" => "\225\181\144"
    | #"n" => "\226\129\191"
    | #"o" => "\225\181\146"
    | #"p" => "\225\181\150"
    | #"q" => "\234\157\176" (* dummy *)
    | #"r" => "\202\179"
    | #"s" => "\203\162"
    | #"t" => "\225\181\151"
    | #"u" => "\225\181\152"
    | #"v" => "\225\181\155"
    | #"w" => "\202\183"
    | #"x" => "\203\163"
    | #"y" => "\202\184"
    | #"z" => "\225\182\187"
    | #"0" => "\226\129\176"
    | #"1" => "\194\185"
    | #"2" => "\194\178"
    | #"3" => "\194\179"
    | #"4" => "\226\129\180"
    | #"5" => "\226\129\181"
    | #"6" => "\226\129\182"
    | #"7" => "\226\129\183"
    | #"8" => "\226\129\184"
    | #"9" => "\226\129\185"
    | _    => "\203\128"
fun superscript_of_string (s : string) =
  String.concat (List.map (superscript_of_char) (String.explode s))
		    
fun subscript_of_int (n : int) =
  case n of
      0 => "\226\130\128"
    | 1 => "\226\130\129" 
    | 2 => "\226\130\130" 
    | 3 => "\226\130\131" 
    | 4 => "\226\130\132" 
    | 5 => "\226\130\133" 
    | 6 => "\226\130\134" 
    | 7 => "\226\130\135" 
    | 8 => "\226\130\136" 
    | 9 => "\226\130\137"
    | _ => ((subscript_of_int (n div 10)) ^ (subscript_of_int (n mod 10)))

(* Just for testing *)
fun string_of_int_exp (n : int) =
  if n > 1000 andalso n mod 1000 = 0
  then (string_of_int (n div 1000)) ^ "e3"
  else string_of_int n
		 
fun string_of_tag_ugly (t : tag) =
  case t of
      Unit  => "()"
    | Int n => string_of_int n
    | Rat x => string_of_rat x
    | Schematic (Clk c_str, n) => "X\226\135\167" ^ (string_of_int n) ^ "\226\135\169" ^ c_str
    | Add (t1, t2) => (string_of_tag_ugly t1) ^ " + " ^ (string_of_tag_ugly t2)
fun string_of_tag_fancy (t : tag) =
  case t of
      Unit  => "()"
    | Int n => string_of_int_exp n
    | Rat x => string_of_rat x
    | Schematic (Clk c_str, n) => "X" ^ subscript_of_int n ^ superscript_of_string c_str
    | Add (t1, t2) => (string_of_tag_fancy t1) ^ " + " ^ (string_of_tag_fancy t2)

(* You may change this parameter, depending on your CLI abilities *)
val string_of_tag = (string_of_tag_fancy)

fun string_of_timestamp_constr c =
  case c of
      Timestamp (Clk cname, n, tag) => "X" ^ subscript_of_int n ^ superscript_of_string cname ^ " = " ^ string_of_tag tag
    | _ => raise UnexpectedMatch
fun string_of_affine_constr c =
  case c of
      Affine (t1, ta, t2, tb) => (string_of_tag t1) ^ " = " ^ (string_of_tag ta) ^ " * " ^  (string_of_tag t2) ^ " + " ^ (string_of_tag tb)
    | _ => raise UnexpectedMatch

(* Print HAA-system *)

fun print_clocks clocks =
  writeln ("\t\t" ^ List.foldr (fn (Clk c, s) => c ^ "\t\t" ^ s) "" clocks)
      
fun print_system_step (step_index: int) (clocks: clock list) (G : system) =
  let
    fun contain_notticksuntil g = List.exists (fn NotTicksUntil _ => true | _ => false) g
    fun contain_notticksfrom g = List.exists (fn NotTicksFrom _ => true | _ => false) g
    (* In SML, Unicode code points must be expressed as decimal UTF-8 *)
    (* Old tick symbol: \226\135\145 to get ⇑ *)
    val TICK_SYM         = "\226\134\145" (* ↑ *) (* ⚡ *)
    val FORBIDDEN_SYM    = "\226\138\152" (* ⊘ *) (* ⛔ *)
    val DEATH_SYM        = "\226\152\160" (* ☠ *) (* \240\159\146\128 to get 💀 *)
    val NONEXISTENCE_SYM = "\226\136\132" (* ∄ *) (* ⛔ *)
    fun constrs_of_clk_instindex c n =
      List.filter (fn Ticks (c', n') => c = c' andalso n = n'
      		      | NotTicks (c', n') => c = c' andalso n = n'
      		      | NotTicksUntil (c', n') => c = c' andalso n < n'
      		      | NotTicksFrom  (c', n') => c = c' andalso n >= n'
		      | Timestamp (c', n', _) => c = c' andalso n = n'
		      | _ => false) G
    fun string_of_constrs_at_clk_instindex clk n g =
      let
        val timestamps = List.filter (fn Timestamp (_, _, tag) => (case tag of Unit => true | Int _ => true | Rat _ => true | _ => false) | _ => false) g
	 val tick_string =
	   if contains (Ticks (clk, n)) g
	   then TICK_SYM
	   else
	     if contain_notticksuntil g 
	     then NONEXISTENCE_SYM 
	     else
	       if contain_notticksfrom g
	       then DEATH_SYM
	       else
	       	 if contains (NotTicks (clk, n)) g
	         then FORBIDDEN_SYM
	         else "" (* Decide whether using empty string or '?' to show unconstrained spot *)
	 val tag_string =
	     case List.find (fn Timestamp (_, _, _) => true | _ => false) timestamps of
		  NONE                         => " "
		| SOME (Timestamp (_, _, tag)) => string_of_tag tag
		| _ => raise UnexpectedMatch
      in tick_string ^ " " ^ tag_string
    end
    fun print_instant n =
      writeln ("[" ^ string_of_int n ^ "]"
		 ^ List.foldl (fn (c, s) => s ^ "\t\t" ^ string_of_constrs_at_clk_instindex c n (constrs_of_clk_instindex c n)) "" clocks)
  in (print_instant step_index)
end

fun print_system (step_index: int) (clocks: clock list) (G : system) =
  let
    fun print_run k =
      if k > step_index
      then ()
      else (print_system_step k clocks G ; print_run (k + 1))
  in print_clocks clocks; print_run 1
end

fun print_affine_constrs (G : system) : unit =
  let
      val affine_constrs =
	   List.filter (fn Affine _ => true | _ => false) G
      val nontriv_timestamps_constrs =
	   List.filter (fn Timestamp (_, _, Schematic _) => true | Timestamp (_, _, Add _) => true | _ => false) G
  in (case (affine_constrs, nontriv_timestamps_constrs) of ([], []) => () | _ => writeln "Affine constraints and non-trivial timestamps:" ;
      List.foldl (fn (c, _) => writeln ("\t" ^ (string_of_affine_constr c))) () affine_constrs ;
      List.foldl (fn (c, _) => writeln ("\t" ^ (string_of_timestamp_constr c))) () nontriv_timestamps_constrs)
  end
  
fun print_floating_ticks (clocks: clock list) (f: TESL_formula) : unit =
  let
    val sporadics = (List.filter (fn fatom => case fatom of Sporadic _ => true | _ => false) f)
    val whentickings = (List.filter (fn fatom => case fatom of WhenTickingOn _ => true | _ => false) f)
    fun string_of_sporadics c =
      List.foldl (fn (Sporadic (Clk clk, tag), s) =>
			if clk = c
			then (string_of_tag tag) ^ ", " ^ s
			else "" ^ s | _ => raise UnexpectedMatch) "" sporadics
    fun string_of_whentickingon c =
      List.foldl (fn (WhenTickingOn (Clk clk_meas_name, tag, Clk clk), s) =>
			if clk = c
			then "(" ^ BOLD_COLOR ^ "when" ^ RESET_COLOR ^ " " ^ (string_of_tag tag) ^ " " ^ BOLD_COLOR ^ "on" ^ RESET_COLOR ^ " " ^ clk_meas_name ^ "), " ^ s
			else "" ^ s | _ => raise UnexpectedMatch) "" whentickings
  in case (sporadics, whentickings) of
      ([], []) => ()
    | _ => (writeln "Floating ticks pending for merge:" ;
     List.app (fn (Clk cname) => writeln ("\t" ^ cname ^ ": " ^ (string_of_sporadics cname) ^ (string_of_whentickingon cname))) clocks)
  end

(* Output snapshots *)
fun print_step_runtime (declared_clocks : clock list) (cfs: TESL_ARS_conf list) (current_step: int) =
  let val () = if current_step = 0
               then ((writeln ("## Runtime diagram:")) ; (print_clocks declared_clocks))
	       else ()
  in case cfs of
    [] => (writeln (BOLD_COLOR ^ RED_COLOR ^ "### Simulation aborted:") ;
		      writeln ("### ERROR: No simulation state to solve" ^ RESET_COLOR))
  | _ => case List.hd cfs of
      (G, step, phi, _) =>
      (print_system_step current_step declared_clocks G)
  end

fun print_dumpres (declared_clocks : clock list) (cfs: TESL_ARS_conf list) = case cfs of
    [] => (writeln (BOLD_COLOR ^ RED_COLOR ^ "### Simulation aborted:") ;
		      writeln ("### ERROR: No simulation state to solve" ^ RESET_COLOR))
  | _ =>
  let
    val number_of_snaps_str = string_of_int (List.length cfs)
    val snap_indx = ref 0
    fun snap_indx_now_str () =
      if (List.length cfs) = 1
      then ""
      else " [" ^ string_of_int ((snap_indx := !snap_indx + 1) ; (!snap_indx)) ^ "/" ^ number_of_snaps_str ^ "]"
    in List.foldl (fn ((G, step, phi, _), _) =>
      let val RUN_COLOR = if has_no_floating_ticks phi then GREEN_COLOR else YELLOW_COLOR in
      (writeln (BOLD_COLOR ^ RUN_COLOR ^ "## Simulation result" ^ snap_indx_now_str() ^ ":") ;
       print_system step declared_clocks G ;
       print RESET_COLOR ;
       print_affine_constrs G ;
       print_floating_ticks declared_clocks phi ;
       writeln "## End") end) () cfs
  end

fun string_of_expr e = case e of
    TypeDecl (c, ty)                                        => (string_of_tag_type ty) ^ "-clock " ^ (string_of_clk c)
  | Sporadic (c, t)                                         => (string_of_clk c) ^ " sporadic " ^ (string_of_tag t)
  | Sporadics (c, tags)                                     => (string_of_clk c) ^ " sporadic " ^ (String.concatWith ", " (List.map (string_of_tag) tags))
  | WhenTickingOn (cmeas, t, ctick)                         => (string_of_clk ctick) ^ " sporadic " ^ (string_of_tag t) ^ " on " ^ (string_of_clk cmeas)
  | TypeDeclSporadics (ty, c, tags)                         => (string_of_tag_type ty) ^ "-clock " ^ (string_of_clk c) ^ " sporadic " ^ (List.foldr (fn (t, s) => (string_of_tag t) ^ ", " ^ s) "" tags)
  | Implies (master, slave)                                 => (string_of_clk master) ^ " implies " ^ (string_of_clk slave)
  | ImpliesNot (master, slave)                              => (string_of_clk master) ^ " implies not " ^ (string_of_clk slave)
  | TagRelation (c1, a, c2, b)                              => "time relation " ^ (string_of_clk c1) ^ " = " ^ (string_of_tag a) ^ " * " ^ (string_of_clk c2) ^ " + " ^ (string_of_tag b)
  | TagRelationCst (c, t)                                   => "time relation " ^ (string_of_clk c) ^ " = " ^ (string_of_tag t)
  | TagRelationRefl (c1, c2)                                => "time relation " ^ (string_of_clk c1) ^ " = " ^ (string_of_clk c2)
  | TimeDelayedBy (master, t, measuring, NONE, slave)             => (string_of_clk master) ^ " time delayed by " ^ (string_of_tag t) ^ " on " ^ (string_of_clk measuring) ^ " implies " ^ (string_of_clk slave)
  | TimeDelayedBy (master, t, measuring, SOME (reset), slave)     => (string_of_clk master) ^ " time delayed by " ^ (string_of_tag t) ^ " on " ^ (string_of_clk measuring) ^ " with reset on " ^ (string_of_clk reset) ^ " implies " ^ (string_of_clk slave)
  | DelayedBy (master, n, counting, slave)                  => (string_of_clk master) ^ " delayed by " ^ (string_of_int n) ^ " on " ^ (string_of_clk counting) ^ " implies " ^ (string_of_clk slave)
  | ImmediatelyDelayedBy (master, n, counting, slave)       => (string_of_clk master) ^ " immediately delayed by " ^ (string_of_int n) ^ " on " ^ (string_of_clk counting) ^ " implies " ^ (string_of_clk slave)
  | FilteredBy (master, s, k, rs, rk, slave)                => (string_of_clk master) ^ " filtered by " ^ (string_of_int s) ^ ", " ^ (string_of_int k) ^ " (" ^ (string_of_int rs) ^ ", " ^ (string_of_int rk) ^ ")* implies " ^ (string_of_clk slave)
  | SustainedFrom (master, beginclk, endclk, slave)            => (string_of_clk master) ^ " sustained from " ^ (string_of_clk beginclk) ^ " to " ^ (string_of_clk endclk) ^ " implies " ^ (string_of_clk slave)
  | SustainedFromImmediately (master, beginclk, endclk, slave) => (string_of_clk master) ^ " sustained immediately from " ^ (string_of_clk beginclk) ^ " to " ^ (string_of_clk endclk) ^ " implies " ^ (string_of_clk slave)
  | SustainedFromWeakly (master, beginclk, endclk, slave)      => (string_of_clk master) ^ " sustained from " ^ (string_of_clk beginclk) ^ " to " ^ (string_of_clk endclk) ^ " weakly implies " ^ (string_of_clk slave)
  | SustainedFromImmediatelyWeakly (master, beginclk, endclk, slave) => (string_of_clk master) ^ " sustained immediately from " ^ (string_of_clk beginclk) ^ " to " ^ (string_of_clk endclk) ^ " weakly implies " ^ (string_of_clk slave)
  | Await (masters, _, _, slave)                            => "await " ^ (List.foldr (fn (clk, s) => (string_of_clk clk) ^ " " ^ s) "" masters) ^ "implies " ^ (string_of_clk slave)
  | WhenClock (m1, m2, slave)                               => (string_of_clk m1) ^ " when " ^ (string_of_clk m2) ^ " implies " ^ (string_of_clk slave)
  | WhenNotClock (m1, m2, slave)                            => (string_of_clk m1) ^ " when not " ^ (string_of_clk m2) ^ " implies " ^ (string_of_clk slave)
  | EveryImplies (master, n_every, n_start, slave)          => (string_of_clk master) ^ " every " ^ (string_of_int n_every) ^ " starting at " ^ (string_of_int n_start) ^  " implies " ^ (string_of_clk slave)
  | NextTo (c, next_c, slave)                               => (string_of_clk c) ^ " next to " ^ (string_of_clk next_c) ^ " implies " ^ (string_of_clk slave)
  | Periodic (c, per, offset)                               => (string_of_clk c) ^ " periodic " ^ (string_of_tag per) ^ " offset " ^ (string_of_tag offset)
  | TypeDeclPeriodic (ty, c, per, offset)                   => (string_of_tag_type ty) ^ "-clock " ^ (string_of_clk c) ^ " periodic " ^ (string_of_tag per) ^ " offset " ^ (string_of_tag offset)
  | Precedes (master, slave, weakly_b)                      => (string_of_clk master) ^ " " ^ (if weakly_b then "weakly " else "strictly ") ^ "precedes " ^ (string_of_clk slave)
  | Excludes (c1, c2)                                       => (string_of_clk c1) ^ " excludes " ^ (string_of_clk c2)
  | Kills (c1, c2) => (string_of_clk c1) ^ " kills " ^ (string_of_clk c2)
  | DirMinstep _	                                       => "<parameter>"
  | DirMaxstep _						    => "<parameter>"
  | DirHeuristic _						    => "<parameter>"
  | DirDumpres						    => "<parameter>"
  | DirScenario _                      			    => "<parameter>"
  | DirDrivingClock _				           => "<parameter>"
  | DirRun							    => "<directive>"
  | DirRunStep						    => "<directive>"
  | DirSelect _						    => "<directive>"
  | DirEventConcretize _					    => "<directive>"
  | DirOutputVCD						    => "<directive>"
  | DirOutputTEX _	   				    => "<directive>"
  | DirExit							    => "<directive>"
  | DirPrint							    => "<directive>"
  | DirHelp							    => "<directive>"
  | _                                                       => "<unknown>"
