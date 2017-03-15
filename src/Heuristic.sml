(* Dummy constants *)
val MAXINT = valOf (Int.maxInt)
val MININT = valOf (Int.minInt)
(* Ranges integers [1 : n] *)
fun range n = let fun aux n' l = if n' = 0 then l else aux (n' - 1) (n' :: l) in aux n [] end;

(** Heuristics are supposed to restrict the universe by choosing configurations that are relevant for simulation.
    They allow to focus on execution of models, and become handy to test your specificaitons.
*)

(* Heuristic 1. In a universe, the heuristic keeps snapshots with the minimal number of floating ticks.
   We need to force the occurence of events as soon as possible. *)
(* UNSAFE *)
fun heuristic_minimize_floating_ticks (cfs : TESL_ARS_conf list) : TESL_ARS_conf list =
  let
    fun nb_floating (frun: TESL_formula) : int =
      List.length (List.filter (fn Sporadic _ => true | WhenTickingOn _ => true | _ => false) frun)
    val min_spor : int =
      List.foldl (fn ((_, _, frun, _), n) => Int.min(n, nb_floating frun)) MAXINT cfs
  in List.filter
	  (fn (_, _, frun, _) => (nb_floating frun) <= min_spor)
	  cfs
  end

(* Heuristic 2. In a universe, the heuristic keeps snapshots with the minimal number of floating ticks.
   We need to force the occurence of events as soon as possible. *)
(* UNSAFE *)
fun heuristic_minimize_sporadic_floating_ticks (cfs : TESL_ARS_conf list) : TESL_ARS_conf list =
  let
    fun nb_floating (frun: TESL_formula) : int =
      List.length (List.filter (fn Sporadic _ => true | _ => false) frun)
    val min_spor : int =
      List.foldl (fn ((_, _, frun, _), n) => Int.min(n, nb_floating frun)) MAXINT cfs
  in List.filter
	  (fn (_, _, frun, _) => (nb_floating frun) <= min_spor)
	  cfs
  end

(* Heuristic 3. Given a clock, if a sporadic was chosen to be merged, then it must be the smallest in the specification.
   Otherwise it will surely lead to inconsistencies. Indeed, if time has progressed enough and a past tick is pending
   for merge, the clock of this tick will never react! The tick will be delayed for merge forever, and never merged in
   the future *)
(* SAFE *)
fun heuristic_no_spurious_sporadics (cfs : TESL_ARS_conf list) : TESL_ARS_conf list =
  List.filter
    (fn (G, _, phi, _) =>
      let val G = reduce G in
      List.all (fn Sporadic (clk, Int n1) => (List.all (fn Timestamp (clk', _, Int n2) => not (clk = clk') orelse (n1 >= n2) | _ => true) G)
                  | _ => true) phi end)
    cfs;

(* Heuristic 4. Same for when tickings. We add a simplifier engine for [when ticking on] formulae to compute tag
   timestamps and then compare. *)
(* SAFE *)
fun simplify_whentickings (G: system) (frun: TESL_formula) =
  let
    fun simplify_tag t: tag = case t of
      Int _ => t
    | Unit => t
    | Schematic (clk, n) => (case List.find (fn Timestamp(clk', n', Int const) => clk = clk' andalso n = n' | _ => false) G of
        NONE => t
      | SOME (Timestamp(_, _, cst)) => cst
      | _ => raise UnexpectedMatch)
    | Add (Int n1, Int n2) => Int (n1 + n2)
    | Add (t1, t2) => Add (simplify_tag t1, simplify_tag t2)
  in List.map (fn
         WhenTickingOn (c1, tag, c2) => WhenTickingOn (c1, lfp (simplify_tag) tag, c2)
	| f => f
     ) frun
  end
fun heuristic_no_spurious_whentickings (cfs : TESL_ARS_conf list) : TESL_ARS_conf list =
  List.filter
    (fn (G, _, phi, _) =>
      let val G = reduce G
	   val phi = simplify_whentickings G phi in
      List.all (fn WhenTickingOn (clk, Int n1, _) => (List.all (fn Timestamp (clk', _, Int n2) => not (clk = clk') orelse (n1 >= n2) | _ => true) G)
                  | _ => true) phi end)
    cfs;

(* Heuristic 5. Rejects runs containing empty instants. Something always have to happen at any step. *)
(* UNSAFE *)
fun heuristic_no_empty_instants (cfs : TESL_ARS_conf list) : TESL_ARS_conf list =
  let fun has_at_least_one_event (G: system) (step : int) =
    List.exists (fn Ticks _ => true | _ => false) (haa_constrs_at_step G step)
  in List.filter
    (fn (G, n, _, _) =>
      List.all (fn n => has_at_least_one_event G n) (range n)
    ) cfs
  end

(* UNSAFE *)
(* Heuristic 6. Maximize reactiveness of clocks *)
fun heuristic_maximize_reactiveness (cfs : TESL_ARS_conf list) : TESL_ARS_conf list =
  let
    val cfs = List.map (fn (G, n, frun, finst) => (reduce G, n, frun, finst)) cfs
    fun nb_ticks (G: system) : int =
      List.length (List.filter (fn Ticks _ => true | _ => false) G)
    val max_ticks : int =
      List.foldl (fn ((G, _, _, _), n) => Int.max(n, nb_ticks G)) MININT cfs
  in List.filter
	  (fn (G, _, _, _) => max_ticks <= (nb_ticks G))
	  cfs
  end


exception Unreferenced_heuristic
fun heuristic_ref_table (f: TESL_atomic) : (TESL_ARS_conf list -> TESL_ARS_conf list) =
  case f of
    DirHeuristic "all"                              => heuristic_maximize_reactiveness
								o heuristic_minimize_floating_ticks
						           	o heuristic_minimize_sporadic_floating_ticks
						           	o heuristic_no_spurious_whentickings
						           	o heuristic_no_spurious_sporadics
						           	o heuristic_no_empty_instants

  | DirHeuristic "minimize_floating_ticks"          => heuristic_minimize_floating_ticks
  | DirHeuristic "no_spurious_sporadics"            => heuristic_no_spurious_sporadics
  | DirHeuristic "no_spurious_whentickings"         => heuristic_no_spurious_whentickings
  | DirHeuristic "no_empty_instants"	          => heuristic_no_empty_instants
  | DirHeuristic "minimize_sporadic_floating_ticks" => heuristic_minimize_sporadic_floating_ticks
  | DirHeuristic "maximize_reactiveness"            => heuristic_maximize_reactiveness
  | DirHeuristic _                                  => raise Unreferenced_heuristic
  | _                                               => raise UnexpectedMatch

fun heuristic_combine
  (spec: TESL_formula)
  : (TESL_ARS_conf list -> TESL_ARS_conf list) =
  case spec of
     [] => (fn x => x)
   | _  => List.foldl
		 (fn (hname, hcurrent) => (heuristic_ref_table hname) o hcurrent)
		 (fn x => x)
		 spec

