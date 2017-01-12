(* Constraint solver for the ARS used in reducing TESL *)
(* The logic structure contains
   1. Booleans fixed-predicates [C \<Up>\<^sub>\<sigma>] and [C \<not>\<Up>\<^sub>\<sigma>]
   2. Affine relations
     - Over integers (\<int>, +, \<times>)
     - With constants [C\<^sub>1], [C\<^sub>2],... and variables [V\<^sub>1], [V\<^sub>2], ...
     - Of the kind [X\<^sub>1 = \<alpha> \<times> X\<^sub>2 + \<beta>], where [X\<^sub>1] and [X\<^sub>2] can be constants or variables
   3. Tag assignments [C \<Down>\<^sub>\<sigma> \<tau>] where [\<tau>] is a constant
   4. Conjunction
*)

(* Whenever there's a ticking predicate, there shouln't be a refutation. Same way for non-ticking predicate. *)
fun check_non_contradictory_ticks (G: system) =
  let
    val ticks      = (List.filter (fn cstr => case cstr of Ticks _ => true | _ => false) G)
    val notticks   = (List.filter (fn cstr => case cstr of NotTicks _ => true | _ => false) G)
  in
    List.all
      (fn Ticks (clk, i) => not (List.exists (fn NotTicks (clk', i') => clk = clk' andalso i = i') notticks))
      ticks
  andalso
    List.all
      (fn NotTicks (clk, i) => not (List.exists (fn Ticks (clk', i') => clk = clk' andalso i = i') ticks))
      notticks
  end;

(* Whenever there's a timestamp constraint at fixed clock, all others with higher instant index also have higher time tags *)
fun check_injectivity_on_timestamps (G: system) =
  let
    val timestamps = List.filter (fn cstr => case cstr of
        Timestamp (_, _, Int _) => true
      | Timestamp (_, _, Unit) => true
      | _           => false) G
  in List.all
      (fn Timestamp (clk, i, tag) => List.all (fn Timestamp (clk', i', tag') => not (clk = clk' andalso i = i') orelse tag = tag') timestamps)
      timestamps
  end;

(* Same as above but for variables. Two timestamps [H \<Down>\<^sub>\<sigma> V] and [H \<Down>\<^sub>\<sigma> V + \<delta>], where [\<delta> \<noteq> 0] *)
fun check_injectivity_on_timestamps_varadd (G: system) =
  let
    val timestamps_var = (List.filter (fn cstr => case cstr of Timestamp (_, _, Schematic _) => true | _ => false) G)
    val timestamps_add = (List.filter (fn cstr => case cstr of Timestamp (_, _, Add (Schematic _, Int _)) => true | _ => false) G)
  in List.all
      (fn Timestamp (clk, i, Schematic _) => List.all (fn Timestamp (clk', i', Add (Schematic (clk'', i''), Int dt)) =>
        not (clk = clk' andalso i = i' andalso clk = clk'' andalso i = i'') orelse dt = 0) timestamps_add)
      timestamps_var
  end;

(* Whenever there's a timestamp constraint at fixed clock, all others with higher instant index also have higher time tags *)
fun check_ascending_chain_on_timestamps (G: system) =
  let
    val timestamps = (List.filter (fn cstr => case cstr of
        Timestamp (_, _, Int _) => true
      | Timestamp (_, _, Unit) => true
      | _           => false) G)
  in List.all
      (fn Timestamp (clk, i, tag) => List.all (fn Timestamp (clk', i', tag') => not (clk = clk' andalso i < i') orelse ::<= (tag, tag')) timestamps)
      timestamps
  end;

(* On a fixed clock, tags have the same type *)
fun check_type_consistency (G: system) =
  let
    val timestamps = (List.filter (fn cstr => case cstr of
        Timestamp (_, _, Int _) => true
      | Timestamp (_, _, Unit) => true
      | _           => false) G)
  in List.all
      (fn Timestamp (clk, i, tag) => List.all (fn Timestamp (clk', i', tag') => not (clk = clk') orelse ::~ (tag, tag')) timestamps)
      timestamps
  end;

(* Eliminate one schematic variable from an affine equation system *)
fun schematic_elim (G: system) (evar: tag) : system =
  let
    val affines   = (List.filter (fn cstr => case cstr of Affine _ => true | _ => false) G)
    val left_occ  = (List.filter (fn cstr => case cstr of Affine (var, _, _, _) => var = evar | _ => false) affines)
    val right_occ = (List.filter (fn cstr => case cstr of Affine (_, _, var, _) => var = evar | _ => false) affines)
    val no_occ    = (List.filter (fn cstr => case cstr of Affine (var, _, var', _) => evar <> var andalso evar <> var' | _ => true) G)
    val eliminated = List.concat (List.map (fn
      Affine (x1, Int a, evar, Int b) => List.map (fn
        Affine (_, Int a', x3, Int b') =>
          Affine (x1, Int (a * a'), x3, Int (a * b' + b))) (@- (left_occ, [Affine (x1, Int a, evar, Int b)]))) right_occ)
  in
    eliminated @ no_occ
  end;

(* Affine variable elimination main step *)
fun schematic_elim_step (G: system) =
  let
    val affines        = (List.filter (fn cstr => case cstr of Affine _ => true | _ => false) G)
    val eliminable_constr =
      List.filter (fn
        Affine (x1, a, x2, b) => List.exists (fn
          Affine (x1', a', x2', b') =>
            x1 = x2') (@-(affines, [Affine(x1, a, x2, b)]))) affines
    val eliminable_vars = List.map (fn Affine (x, _, _, _) => x) eliminable_constr
  in
    if is_empty (eliminable_vars)
    then G
    else schematic_elim G (List.nth (eliminable_vars, 0))
  end;
fun schematic_elim (G: system) =
  lfp (schematic_elim_step) G

(* Apply a complete substitution where tag [t1] is substituted by tag [t2] in constraint system [G] *)
fun apply_tag_substition ((t1, t2): tag * tag) (G: system) =
  List.map (fn 
      Timestamp (clk, k, t)     => if t = t1 then Timestamp (clk, k, t2) else Timestamp (clk, k, t)
    | Affine (ltag, a, rtag, b) =>
      if ltag = t1 andalso rtag = t1
      then Affine (t2, a, t2, b)
      else if ltag = t1
        then Affine (t2, a, rtag, b)
        else if rtag = t1
          then Affine (ltag, a, t2, b)
          else Affine (ltag, a, rtag, b)
    | x => x) G;

(* Constants propagation is possible whenever there exists:
   - Two timestamps [H \<Down>\<^sub>\<sigma> X] and [H \<Down>\<^sub>\<sigma> C], where [X] a variable and [C] is a constant (by injectivity)
   - Affine relation [X = a * C + b] (by (+, \<times>) closure of Z)
   - Affine relation [C = a * X + b] if [X] has a solution in Z
   - Affine relation with integer fixpoint [X = a * X + b] if [X] has a solution in Z *)
fun constants_propagation_candidates (G: system) =
  let
    val timestamp_var_right = (List.filter (fn cstr => case cstr of Timestamp (_, _, Schematic _) => true | _ => false) G)
    val timestamp_cst_right = (List.filter (fn cstr => case cstr of Timestamp (_, _, Int _) => true | _ => false) G)
    val timestamp_unifiers = List.concat (List.concat (List.map (fn
      Timestamp (clk, k, Schematic (_, _)) => (List.map (fn
        Timestamp (clk', k', Int i) => (
          if clk = clk' andalso k = k'
          then [(Schematic (clk, k), Int i)]
          else [])) timestamp_cst_right)) timestamp_var_right))
    val affine_var_left = List.filter (fn cstr => case cstr of Affine (Schematic _, Int _, Int _, Int _) => true | _ => false) G
    val affine_cst_left = List.filter (fn cstr => case cstr of Affine (Int _, Int _, Schematic _, Int _) => true | _ => false) G
    val affine_left_unifiers =
      List.map (fn Affine (Schematic (clk, k), Int a, Int i, Int b) => (Schematic (clk, k), Int (a * i + b))) affine_var_left
    val affine_right_unifiers =
      List.concat (List.map (fn Affine (Int i, Int a, X as Schematic _, Int b) =>
        if (i - b) mod a = 0
        then [(X, Int ((i - b) div a))]
        else []
        ) affine_cst_left)
    val affine_fixpoint_var = List.filter (fn cstr => case cstr of Affine (X1 as Schematic _, Int _, X2 as Schematic _, Int _) => X1 = X2 | _ => false) G
    val affine_fixpoint_unifiers =
      List.concat (List.map (fn Affine (X as Schematic _, Int a, Schematic _, Int b) =>
        if b mod (1 - a) = 0
        then [(X, Int (b div (1 - a)))]
        else []
        ) affine_fixpoint_var)
  in
    timestamp_unifiers @ affine_left_unifiers @ affine_right_unifiers @ affine_fixpoint_unifiers
  end;

(* Constant propagation main step *)
fun constants_propagation_step (G: system) =
  List.foldl (fn (subst, h) => apply_tag_substition subst h) G (constants_propagation_candidates G);
fun constants_propagation (G: system) =
  lfp constants_propagation_step G

(*
val h0 = [
  Timestamp (Clk 1, 0, Schematic (Clk 1, 0)),
  Timestamp (Clk 1, 0, Int 999),
  Timestamp (Clk 2, 0, Schematic (Clk 2, 0)),
  Timestamp (Clk 2, 0, Int 888),
  Affine (Schematic (Clk 2, 0), Int ~1, Schematic (Clk 1, 0), Int ~1),
  Affine (Schematic (Clk 3, 0), Int ~1, Int ~1, Int ~1),
  Affine (Schematic (Clk 4, 0), Int ~1, Schematic (Clk 3, 0), Int ~1)];
val candidates_prop_h0 = constants_propagation_candidates h0;
val candidates_prop_h0 = constants_propagation_candidates h0;
val h1 = constants_propagation_step h0;
val h2 = constants_propagation_step h1;
val h3 = constants_propagation_step h2;

constants_propagation h0;
*)

(* Affine equations with (only) constants elimination *)
fun constant_affine_eqns_elim (G: system) : system =
  List.filter (fn Affine (Int t1, Int a, Int t2, Int b) => not (t1 = a * t2 + b)| _ => true) G

(* Equation [X = a * X + b] has a solution in Z iff [(1 - a)] is dividable by [b] *)
fun check_fixpoint_affeqns (G: system) : bool =
  List.all (fn Affine (X1 as Schematic _, Int a, X2 as Schematic _, Int b) => not (X1 = X2) orelse (b mod (1 - a)) = 0 | _ => true) G

(* Equation with constants only [C1 = a * C2 + b] *)
fun check_constants_affeqns (G: system) : bool =
  List.all (fn Affine (Int x1, Int a, Int x2, Int b) => x1 = a * x2 + b | _ => true) G

(* An equation [C = a * X + b] has a solution in Z iff [C - a] is dividable by [b] *)
fun check_varright_affeqns (G: system) : bool =
  List.all (fn Affine (Int C, Int a, Schematic _, Int b) => (C - b) mod a = 0 | _ => true) G

(* For equations of the general kind [X1 = a * X2 + b], all of them are together satisfiable
   when they all share no common variables *)
fun check_no_shared_var_affeqns (G: system) =
  let 
    val affine_var_var = (List.filter (fn cstr => case cstr of Affine (Schematic _, _, Schematic _, _) => true | _ => false) G)
    val vars_in_affine_var_var = List.concat (List.map (fn Affine (v1 as Schematic _, _, v2 as Schematic _, _) => [v1, v2] | _ => []) affine_var_var)
    val vars_cardinalities = List.map (fn v => (v, List.length (List.filter (fn v' => v = v') vars_in_affine_var_var))) vars_in_affine_var_var
  in List.all (fn (_, n) => n <= 1) vars_cardinalities
  end;

(* check_no_shared_var_affeqns h0; *)
val g0 = [
  Affine (Schematic(Clk 1, 0), Int 1, Schematic(Clk 2, 0), Int 2),
  Affine (Schematic(Clk 2, 0), Int 3, Schematic(Clk 3, 0), Int 4),
  Affine (Schematic(Clk 3, 0), Int 5, Schematic(Clk 1, 0), Int 6)];
val g1 = schematic_elim_step g0;
val g2 = schematic_elim_step g1;
val g3 = schematic_elim_step g2;
val g4 = schematic_elim_step g3;
val g5 = schematic_elim_step g4;
(* check_fixpoint_equations g0; *)

(* BUG: Need a predicate to know if the system is reduced to be decided *)
(* need to check that tags a and b are constants *)
(*
    val reduced_G = lfp (equation_reduce_step) G
*)
(* TODO:
  - Schematic variable renaming
  - Eval propagate add of constants
*)

(* Main steps of the solver
   
   Check SAT
     0. Remove redundant constraints
     1. Constant propagation
     2. Affine variables elimination
     REPEAT UNTIL FIXPOINT
   REPEAT UNTIL NORMAL FORM

*)
fun decide (G: system) : bool =
           check_non_contradictory_ticks G
  andalso check_injectivity_on_timestamps G
  andalso check_injectivity_on_timestamps_varadd G
  andalso check_ascending_chain_on_timestamps G
  andalso check_type_consistency G
  andalso check_fixpoint_affeqns G
  andalso check_constants_affeqns G
  andalso check_no_shared_var_affeqns G
  andalso check_varright_affeqns G
;

fun SAT (G: system) : bool =
  let
    val G_prop_and_elim_until_fp =
      lfp (fn G => constant_affine_eqns_elim (schematic_elim (constants_propagation (uniq G)))) G
  in decide G_prop_and_elim_until_fp
  end;

fun context_SAT ((G, _, _, _) : TESL_ARS_conf) =
  SAT G;

val gg0 = [
  Ticks (Clk 1, 0), Timestamp (Clk 1, 0, Int 5555),
  Timestamp (Clk 1, 0, Schematic (Clk 1, 0)), Timestamp (Clk 1, 0, Schematic (Clk 2, 0)), Affine (Schematic (Clk 1, 0), Int 1, Schematic (Clk 2, 0), Int 0),
  Timestamp (Clk 2, 0, Schematic (Clk 2, 0)),

  Timestamp (Clk 2, 1, Add (Schematic (Clk 2, 0), Int (1111))), Ticks (Clk 3, 1),
  Timestamp (Clk 1, 1, Schematic (Clk 1, 1)), Timestamp (Clk 1, 1, Schematic (Clk 2, 1)),
  NotTicks (Clk 1, 1)
];
SAT gg0;

(*val gg1 = constants_propagation gg0;
val gg2 = schematic_elim_step gg1;

val gg3 = constants_propagation gg2;
val gg4 = schematic_elim_step gg3;

gg2 = gg4;
*)