(* Heuristic 1. The heuristic is supposed to restrict the universe by choosing configurations that are relevant for simulation *)
(* UNSAFE *)
fun heuristic_minsporadic (cfs : TESL_ARS_conf list) : TESL_ARS_conf list =
  let
    fun min_pending_spor (cfs : TESL_ARS_conf list) : int =
    let
      val cfs_with_spor = List.filter (fn (_, _, frun, _) => List.exists (fn Sporadic _ => true | _ => false) frun) cfs
      val selected_cfs_min_start = case (List.nth (cfs_with_spor, 0)) of (_, _, frunsel, _) =>
        List.length (List.filter (fn Sporadic _ => true | _ => false) frunsel) in
    List.foldl (fn ((_, _, frun, _), n) =>
      let val nb_spor = List.length (List.filter (fn Sporadic _ => true | _ => false) frun) in
      if nb_spor >= n then n else nb_spor end) selected_cfs_min_start cfs_with_spor end
    val min_spor = min_pending_spor cfs
  in List.filter
      (fn (_, _, frun, _) => (List.length (List.filter (fn Sporadic _ => true | _ => false) frun)) <= min_spor + 1) (* TWEAK PARAMETER *)
      cfs end

(* Heuristic 2. Given a clock, if a sporadic was chosen to be merged, then it must be the smallest in the specification.
   Otherwise it will eventually lead to inconsistencies *)
fun heuristic_monotonic_sporadic (cfs : TESL_ARS_conf list) : TESL_ARS_conf list =
  List.filter
    (fn (G, _, phi, _) =>
      let val G = reduce G in
      List.all (fn Sporadic (clk, Int n1) => (List.all (fn Timestamp (clk', _, Int n2) => not (clk = clk') orelse (n1 >= n2) | _ => true) G)
                  | _ => true) phi end)
    cfs;
