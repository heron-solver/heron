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
print ("Usage: " ^ CommandLine.name () ^ " [@mpl procs N --] [--use FILE.tesl] [--runtime-print]");
print "\n\n";
print "Copyright (c) 2020, Universit\195\169 Paris-Saclay / CNRS\n";
print "Please cite: H. Nguyen Van, T. Balabonski, F. Boulanger, C. Keller, B. Valiron, B. Wolff.\n";
print "             Formal Modeling and Analysis of Timed Systems (LNCS, volume 10419), pp 318-334.\n";
print "\n";
print (BOLD_COLOR ^ "Tag and clock expressions (for time relations):\n" ^ RESET_COLOR); 
print "  [CLOCK EXPR] = \u001B[1m(\u001B[0m[CLOCK EXPR]\u001B[1m)\u001B[0m\n";
print "  [CLOCK EXPR] = [TAG]\n"; 
print "  [CLOCK EXPR] = [FUNCTION NAME] \u001B[1m(\u001B[0m[CLOCK], [CLOCK]...\u001B[1m)\u001B[0m\n"; 
print "  [CLOCK EXPR] = [CLOCK EXPR] + [CLOCK EXPR]\n"; 
print "  [CLOCK EXPR] = [CLOCK EXPR] - [CLOCK EXPR]\n"; 
print "  [CLOCK EXPR] = [CLOCK EXPR] * [CLOCK EXPR]\n"; 
print "  [CLOCK EXPR] = [CLOCK EXPR] / [CLOCK EXPR]\n"; 
print "  [CLOCK EXPR] = \u001B[1mpre\u001B[0m [CLOCK]\n"; 
print "  [CLOCK EXPR] = \u001B[1m[\u001B[0m [TAG]+ \u001B[1m] ->\u001B[0m [CLOCK]\n"; 
print "  [CLOCK EXPR] = \u001B[1md\u001B[0m [CLOCK]\n"; 
print "\n"; 
print (BOLD_COLOR ^ "TESL language expressions:\n" ^ RESET_COLOR); 
print "  [\u001B[1mint\u001B[0m | \u001B[1mrational\u001B[0m | \u001B[1munit\u001B[0m]\u001B[1m-clock\u001B[0m [CLOCK]\n"; 
print "  \u001B[1mtime relation\u001B[0m [CLOCK EXPR] \u001B[1m=\u001B[0m [CLOCK EXPR]\n"; 
print "  [CLOCK] \u001B[1msporadic\u001B[0m [TAG]+\n"; 
print "  [CLOCK] \u001B[1msporadic\u001B[0m [TAG] \u001B[1mon\u001B[0m [CLOCK]\n"; 
print "  [CLOCK] \u001B[1mperiodic\u001B[0m [TAG] (\u001B[1moffset\u001B[0m [TAG])\n"; 
print "  [CLOCK] (/\\ [CLOCK])+ \u001B[1mimplies\u001B[0m [CLOCK] (\\/ [CLOCK])+\n"; 
print "  [CLOCK] \u001B[1mimplies not\u001B[0m [CLOCK]\n"; 
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
print (BOLD_COLOR ^ "Extensions:\n" ^ RESET_COLOR); 
print "  \u001B[1mrational-quantity\u001B[0m [CLOCK]\n";
print "  [CLOCK] \u001B[1m[strictly | weakly] precedes\u001B[0m [CLOCK]\n"; 
print "  [CLOCK] \u001B[1mexcludes\u001B[0m [CLOCK]\n"; 
print "  [CLOCK] \u001B[1mkills\u001B[0m [CLOCK]\n";
print "  [CLOCK] \u001B[1mimplies time relation\u001B[0m [CLOCK] = [CLOCK]\n";
print "\n"; 
print "  Real functions interpreted as in ISO C's math.h:\n"; 
print "  pi, e, sqrt, sin, cos, tan, asin, acos, atan, atan2, exp, pow, ln, log10, sinh, cosh, tanh\n"; 
print "\n"; 
print (BOLD_COLOR ^ "Run parameters:\n" ^ RESET_COLOR);  
print "  \u001B[1m@minstep\u001B[0m [INT]                    define the number of minimum run steps\n"; 
print "  \u001B[1m@maxstep\u001B[0m [INT]                    define the number of maximum run steps\n"; 
print "  \u001B[1m@policy\u001B[0m [NAME]                    load a simulation policy among:\n";
print "                                      \u001B[1masap\u001B[0m\n"; 
print "                                      \u001B[1mminimize_ticks\u001B[0m\n";
print "                                      \u001B[1mspeedup_event_occ\u001B[0m\n";
print "                                      \u001B[1mminimize_floating_ticks\u001B[0m\n";
print "                                      \u001B[1mminimize_unsolved_affine\u001B[0m\n";
print "                                      \u001B[1mno_empty_instants\u001B[0m\n";
print "                                      \u001B[1mmaximize_reactiveness\u001B[0m\n";
print "                                      \u001B[1mmaximize_absence\u001B[0m\n";
print "                                      \u001B[1mminimize_absence\u001B[0m\n";
print "  \u001B[1m@dumpres\u001B[0m                          option to display the results after @run\n"; 
print "  \u001B[1m@scenario\u001B[0m (\u001B[1mstrict\u001B[0m) [INT] [CLOCK]+ refine snapshots with instantaneous scenario\n"; 
print "  \u001B[1m@scenario\u001B[0m (\u001B[1mstrict\u001B[0m) \u001B[1mnow\u001B[0m [CLOCK]+   refine snapshots of last simulation step\n"; 
print "  \u001B[1m@scenario\u001B[0m (\u001B[1mstrict\u001B[0m) \u001B[1mnext\u001B[0m [CLOCK]+  refine snapshots of next simulation step\n"; 
print "  \u001B[1m@select\u001B[0m [#[INT] | 0x[INT]]        select by keeping only one simulation state\n"; 
print "  \u001B[1m@driving-clock\u001B[0m [CLOCK]+           declare driving clocks\n"; 
print "\n"; 
print (BOLD_COLOR ^ "Interactive commands:\n" ^ RESET_COLOR);  
print "  \u001B[1m@exit\u001B[0m                             exit Heron\n"; 
print "  \u001B[1m@run\u001B[0m (\u001B[1muntil\u001B[0m [CLOCK]+)             run the specification until model found\n"; 
print "  \u001B[1m@step\u001B[0m                             run the specification for one step\n"; 
print "  \u001B[1m@stutter\u001B[0m                          stutters the last snapshot instant\n"; 
print "  \u001B[1m@event-concretize\u001B[0m                 concretize ticks/tags of driving clocks\n"; 
print "  \u001B[1m@print\u001B[0m                            display the current snapshots\n"; 
print "  \u001B[1m@output\u001B[0m \u001B[1mvcd\u001B[0m/\u001B[1mtikz\u001B[0m/\u001B[1mtex\u001B[0m/\u001B[1mpdf\u001B[0m/\u001B[1mcsv\u001B[0m      export to VCD/TikZ/LaTeX/PDF/CSV file with clock selection\n"; 
print "  \u001B[1m@help\u001B[0m                             display the list of commands\n";
print "\n"; 
print (BOLD_COLOR ^ "Multicore support (only supported when compiled with MPL):\n" ^ RESET_COLOR);
print "  To use the solver on multiple cores, you need to use MPL-level argument procs.\n";
print "  For instance, with 4 cores:\n";
print ("  $ " ^ (CommandLine.name ()) ^ " @mpl procs 4 -- -use examples/basic/FirstExample.tesl\n")
)

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
    | Rat x => string_of_rat x GEN 10
    | Schematic (Clk c_str, n) => "tvar\226\135\167" ^ (string_of_int n) ^ "\226\135\169" ^ c_str
    | Add (t1, t2) => (string_of_tag_ugly t1) ^ " + " ^ (string_of_tag_ugly t2)
fun string_of_tag_fancy (t : tag) =
  case t of
      Unit  => "()"
    | Int n => string_of_int_exp n
    | Rat x => string_of_rat x GEN 10
    | Schematic (Clk c_str, n) => "tvar" ^ subscript_of_int n ^ superscript_of_string c_str
    | Add (t1, t2) => (string_of_tag_fancy t1) ^ " + " ^ (string_of_tag_fancy t2)

(* You may change this parameter, depending on your CLI abilities *)
val string_of_tag = (string_of_tag_fancy)

fun string_of_tags tlist =
    String.concatWith " " (List.map (string_of_tag) tlist)    

fun string_of_timestamp_primitive c =
  case c of
      Timestamp (Clk cname, n, tag) => "tvar" ^ subscript_of_int n ^ superscript_of_string cname ^ " = " ^ string_of_tag tag
    | _ => raise UnexpectedMatch
fun string_of_affine_primitive c =
  case c of
      Affine (t1, ta, t2, tb) => (string_of_tag t1) ^ " = " ^ (string_of_tag ta) ^ " * " ^  (string_of_tag t2) ^ " + " ^ (string_of_tag tb)
    | AffineRefl (t1, t2) => (string_of_tag t1) ^ " = " ^ (string_of_tag t2)
    | FunRel (t, Fun (fname), tlist) => (string_of_tag t) ^ " = " ^ fname ^ " (" ^ (string_of_tags tlist) ^ ")"
    | _ => raise UnexpectedMatch

(* Print a run context (past) *)

fun print_clocks clocks =
  writeln ("\t\t" ^ List.foldr (fn (Clk c, s) => c ^ "\t\t" ^ s) "" clocks)
      
fun print_context_step (step_index: int) (clocks: clock list) (G : context) =
  let
    fun contain_notticksuntil g = List.exists (fn NotTicksUntil _ => true | _ => false) g
    fun contain_notticksfrom g = List.exists (fn NotTicksFrom _ => true | _ => false) g
    (* In SML, Unicode code points must be expressed as decimal UTF-8 *)
    (* Old tick symbol: \226\135\145 to get ⇑ *)
    val TICK_SYM         = "\226\134\145" (* ↑ *) (* ⚡ *)
    val FORBIDDEN_SYM    = "\226\138\152" (* ⊘ *) (* ⛔ *)
    val DEATH_SYM        = "\226\152\160" (* ☠ *) (* \240\159\146\128 to get 💀 *)
    val NONEXISTENCE_SYM = "\226\136\132" (* ∄ *) (* ⛔ *)
    fun primitives_of_clk_instindex c n =
      List.filter (fn Ticks (c', n') => c = c' andalso n = n'
      		      | NotTicks (c', n') => c = c' andalso n = n'
      		      | NotTicksUntil (c', n') => c = c' andalso n < n'
      		      | NotTicksFrom  (c', n') => c = c' andalso n >= n'
		      | Timestamp (c', n', _) => c = c' andalso n = n'
		      | _ => false) G
    fun string_of_primitives_at_clk_instindex clk n g =
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
		 ^ List.foldl (fn (c, s) => s ^ "\t\t" ^ string_of_primitives_at_clk_instindex c n (primitives_of_clk_instindex c n)) "" clocks)
  in (print_instant step_index)
end

fun print_context (step_index: int) (clocks: clock list) (G : context) =
  let
    fun print_run k =
      if k > step_index
      then ()
      else (print_context_step k clocks G ; print_run (k + 1))
  in print_clocks clocks; print_run 1
end

fun print_affine_primitives (G : context) : unit =
  let
      val affine_primitives =
	   List.filter (fn Affine _ => true | AffineRefl _ => true | FunRel _ => true | _ => false) G
      val nontriv_timestamps_primitives =
	   List.filter (fn Timestamp (_, _, Schematic _) => true | Timestamp (_, _, Add _) => true | _ => false) G
  in (case (affine_primitives, nontriv_timestamps_primitives) of ([], []) => () | _ => writeln "Affine constraints and non-trivial timestamps:" ;
      List.foldl (fn (c, _) => writeln ("\t" ^ (string_of_affine_primitive c))) () affine_primitives ;
      List.foldl (fn (c, _) => writeln ("\t" ^ (string_of_timestamp_primitive c))) () nontriv_timestamps_primitives)
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
fun print_step_runtime (declared_clocks : clock list) (cfs: TESL_conf list) (current_step: int) =
  let val () = if current_step = 0
               then ((writeln ("## Runtime diagram:")) ; (print_clocks declared_clocks))
	       else ()
  in case cfs of
    [] => (writeln (BOLD_COLOR ^ RED_COLOR ^ "### Simulation aborted:") ;
		      writeln ("### ERROR: No simulation state to solve" ^ RESET_COLOR))
  | _ => (case List.hd cfs of
        (_, 0, _, _)    => ()
      | (G, step, phi, _) => (print_context_step current_step declared_clocks G))
  end

fun print_dumpres
  (declared_clocks: clock list)
  (cfs: TESL_conf list) = case cfs of
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
    val _ = List.foldl (fn ((G, step, phi, psi), _) =>
      let val RUN_COLOR = if has_no_floating_ticks phi then GREEN_COLOR else YELLOW_COLOR in
      (writeln (BOLD_COLOR ^ RUN_COLOR ^ "## Simulation result" ^ snap_indx_now_str() ^ " [0x" ^ (Hash.str_hash_of_TESL_conf (G, step, phi, psi)) ^ "]:") ;
       print_context step declared_clocks G ;
       print RESET_COLOR ;
       print_affine_primitives G ;
       print_floating_ticks declared_clocks phi ;
       writeln "## End") end) () cfs
    val _ = if Hash.find_collision cfs
	     then writeln (BOLD_COLOR ^ YELLOW_COLOR ^ "### WARNING: An hash collision exists." ^ RESET_COLOR)
	     else ()
  in ()
  end

fun string_of_clk_rel symb = case symb of
  ClkExprEqual => "="

fun string_of_clk_expr e = case e of
    ClkCst (t)                     => string_of_tag t
  | ClkName (Clk cname)            => cname
  | ClkDer (e')                    => "d (" ^ (string_of_clk_expr e') ^ ")"
  | ClkPre (e')                    => "pre (" ^ (string_of_clk_expr e') ^ ")"
  | ClkFby (tags, e')               => "[" ^ (List.foldl (fn (t, str) => str ^ (string_of_tag t) ^ " ") "" tags) ^ "] -> " ^ (string_of_clk_expr e')
  | ClkPlus (cexp1, cexp2)         => "(" ^ (string_of_clk_expr cexp1) ^ " + " ^ (string_of_clk_expr cexp2) ^ ")"
  | ClkMinus (cexp1, cexp2)         => "(" ^ (string_of_clk_expr cexp1) ^ " - " ^ (string_of_clk_expr cexp2) ^ ")"
  | ClkMult (cexp1, cexp2)         => "(" ^ (string_of_clk_expr cexp1) ^ " * " ^ (string_of_clk_expr cexp2) ^ ")"
  | ClkDiv (cexp1, cexp2)         => "(" ^ (string_of_clk_expr cexp1) ^ " / " ^ (string_of_clk_expr cexp2) ^ ")"
  | ClkFun (Fun (fname), exp_list) => fname ^ " (" ^ (String.concatWith ", " (List.map (string_of_clk_expr) exp_list)) ^ ")"

fun string_of_expr e = case e of
    TypeDecl (c, ty, mono)                                  => (string_of_tag_type ty)
									^ (if mono then "-clock " else "-quantity ")
									^ (string_of_clk c)
  | Sporadic (c, t)                                         => (string_of_clk c) ^ " sporadic " ^ (string_of_tag t)
  | Sporadics (c, tags)                                     => (string_of_clk c) ^ " sporadic " ^ (String.concatWith ", " (List.map (string_of_tag) tags))
  | WhenTickingOn (cmeas, t, ctick)                         => (string_of_clk ctick) ^ " sporadic " ^ (string_of_tag t) ^ " on " ^ (string_of_clk cmeas)
  | TypeDeclSporadics (ty, c, tags, mono)                   => (string_of_tag_type ty)
									^ (if mono then "-clock " else "-quantity ")
									^ (string_of_clk c)
									^ " sporadic " ^ (List.foldr (fn (t, s) => (string_of_tag t) ^ ", " ^ s) "" tags)
  | Implies (master, slave)                                 => (string_of_clk master) ^ " implies " ^ (string_of_clk slave)
  | ImpliesGen (masters, slaves)                            => (StringMore.concat " /\\ " (List.map (string_of_clk) masters)) ^ " implies " ^ (StringMore.concat " \\/ " (List.map (string_of_clk) slaves))
  | ImpliesNot (master, slave)                              => (string_of_clk master) ^ " implies not " ^ (string_of_clk slave)
  | TagRelation (relsymb, cexp1, cexp2)                     => "time relation " ^ (string_of_clk_expr cexp1) ^ " " ^ (string_of_clk_rel relsymb) ^ " " ^ (string_of_clk_expr cexp2)
  | TagRelationAff (c1, a, c2, b)                           => "time relation " ^ (string_of_clk c1) ^ " = " ^ (string_of_tag a) ^ " * " ^ (string_of_clk c2) ^ " + " ^ (string_of_tag b)
  | TagRelationCst (c, t)                                   => "time relation " ^ (string_of_clk c) ^ " = " ^ (string_of_tag t)
  | TagRelationRefl (c1, c2)                                => "time relation " ^ (string_of_clk c1) ^ " = " ^ (string_of_clk c2)
  | TagRelationClk (c1, ca, c2, cb)                         => "time relation " ^ (string_of_clk c1) ^ " = " ^ (string_of_clk ca) ^ " * " ^ (string_of_clk c2) ^ " + " ^ (string_of_clk cb)
  | TagRelationPre (c1, c2)                                 => "time relation " ^ (string_of_clk c1) ^ " = pre " ^ (string_of_clk c2)
  | TagRelationFby (c1, tags, c2)                           => "time relation " ^ (string_of_clk c1) ^ " = [" ^ (List.foldl (fn (t, str) => str ^ (string_of_tag t) ^ " ") "" tags) ^ "] -> " ^ (string_of_clk c2)
  | TagRelationFun (c, Fun(fname), clist)                   => "time relation " ^ (string_of_clk c) ^ " = " ^ fname ^ " (" ^ (string_of_clks_comma clist) ^ ")"
  | TagRelationDer (c1, c2)                                 => "time relation " ^ (string_of_clk c1) ^ " = d " ^ (string_of_clk c2)
  | TagRelationReflImplies (c1, c2, c3)                     => (string_of_clk c1) ^ " implies time relation " ^ (string_of_clk c2) ^ " = " ^ (string_of_clk c3)
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
  | DirRun _ 						    => "<directive>"
  | DirRunStep						    => "<directive>"
  | DirStutter						    => "<directive>"
  | DirSelect _						    => "<directive>"
  | DirEventConcretize _					    => "<directive>"
  | DirOutputVCD						    => "<directive>"
  | DirOutputTEX _	   				    => "<directive>"
  | DirExit							    => "<directive>"
  | DirPrint _  					    => "<directive>"
  | DirHelp							    => "<directive>"
  | _                                                       => "<unknown>"
