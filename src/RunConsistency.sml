(**
   Module RunConsistency

   Author : Hai Nguyen Van
            LRI, Université Paris-Sud/CNRS
   
   The copyright to this code is held by Laboratoire de Recherche en
   Informatique, Université Paris-Sud/CNRS. All rights reserved. This
   file is distributed under the MIT License.
*)

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


(* Returns primitives for a given specific step *)
fun haa_constrs_at_step (G: system) (step: int) =
  List.filter (fn
      Timestamp (_, step', _) => step = step'
    | Ticks (_, step')        => step = step'
    | NotTicks (_, step')     => step = step'
    | Affine _                => false
) G
exception UnexpectedMatch2

(* Whenever there's a ticking predicate, there shouln't be a refutation. Same way for non-ticking predicate. *)
fun check_non_contradictory_ticks (G: system) =
  let
    val ticks      = (List.filter (fn cstr => case cstr of Ticks _ => true | _ => false) G)
    val notticks   = (List.filter (fn cstr => case cstr of NotTicks _ => true | _ => false) G)
    val notticksuntil = (List.filter (fn cstr => case cstr of NotTicksUntil _ => true | _ => false) G)
    val notticksfrom  = (List.filter (fn cstr => case cstr of NotTicksFrom _ => true | _ => false) G)
  in
    List.all
      (fn Ticks (clk, i) => not (List.exists (fn NotTicks (clk', i') => clk = clk' andalso i = i' | _ => raise UnexpectedMatch2) notticks)
      	  	      	    andalso (not (List.exists (fn NotTicksUntil (clk', i') => clk = clk' andalso i < i' | _ => raise UnexpectedMatch2) notticksuntil))
      	  	      	    andalso (not (List.exists (fn NotTicksFrom  (clk', i') => clk = clk' andalso i >= i' | _ => raise UnexpectedMatch2) notticksfrom))
      	  | _ => raise UnexpectedMatch2)
      ticks
  andalso
    List.all
      (fn NotTicks (clk, i) => not (List.exists (fn Ticks (clk', i') => clk = clk' andalso i = i' | _ => raise UnexpectedMatch2) ticks) | _ => raise UnexpectedMatch2)
      notticks
  end;

(* Whenever there are two timestamp constraints instantaneously at fixed clock, their tags are same *)
fun check_injectivity_on_timestamps (G: system) =
  let
    val timestamps = List.filter (fn cstr => case cstr of
        Timestamp (_, _, Unit) => true
      | Timestamp (_, _, Int _) => true
      | Timestamp (_, _, Rat _) => true
      | _           => false) G
  in List.all
      (fn Timestamp (clk, i, tag) => List.all (fn Timestamp (clk', i', tag') => not (clk = clk' andalso i = i') orelse tag = tag' | _ => raise UnexpectedMatch) timestamps | _ => raise UnexpectedMatch)
      timestamps
  end;

(* Same as above but for variables. Two timestamps [H \<Down>\<^sub>\<sigma> V] and [H \<Down>\<^sub>\<sigma> V + \<delta>], where [\<delta> \<noteq> 0] *)
fun check_injectivity_on_timestamps_varadd (G: system) =
  let
    val timestamps_var = (List.filter (fn cstr => case cstr of Timestamp (_, _, Schematic _) => true | _ => false) G)
    val timestamps_add = (List.filter (fn cstr => case cstr of Timestamp (_, _, Add (Schematic _, tag)) =>
      (case tag of Int _ => true | Rat _ => true | _ => false)
    | _ => false) G)
  in List.all
      (fn Timestamp (clk, i, Schematic _) => List.all (fn
          Timestamp (clk', i', Add (Schematic (clk'', i''), Int dt)) =>
            not (clk = clk' andalso i = i' andalso clk = clk'' andalso i = i'') orelse dt = 0
        | Timestamp (clk', i', Add (Schematic (clk'', i''), Rat dt)) =>
            not (clk = clk' andalso i = i' andalso clk = clk'' andalso i = i'') orelse =/ (dt, rat_zero)
        | _ => raise UnexpectedMatch) timestamps_add | _ => raise UnexpectedMatch)
      timestamps_var
  end;

(* Whenever there's a timestamp constraint at fixed clock, all others with higher instant index also have higher time tags *)
fun check_ascending_chain_on_timestamps (G: system) =
  let
    val timestamps = (List.filter (fn cstr => case cstr of
        Timestamp (_, _, Unit)  => true
      | Timestamp (_, _, Int _) => true
      | Timestamp (_, _, Rat _) => true
      | _           => false) G)
  in List.all
      (fn Timestamp (clk, i, tag) => List.all (fn Timestamp (clk', i', tag') => not (clk = clk' andalso i < i') orelse ::<= (tag, tag') | _ => raise UnexpectedMatch) timestamps | _ => raise UnexpectedMatch)
      timestamps
  end;

(* On a fixed clock, tags have the same type *)
fun check_type_consistency (G: system) =
  let
    val timestamps = (List.filter (fn cstr => case cstr of
        Timestamp (_, _, Unit) => true
      | Timestamp (_, _, Int _) => true
      | Timestamp (_, _, Rat _) => true
      | _           => false) G)
  in List.all
      (fn Timestamp (clk, _, tag) =>
          List.all (fn Timestamp (clk', _, tag') => not (clk = clk') orelse ::~ (tag, tag') | _ => raise UnexpectedMatch) timestamps | _ => raise UnexpectedMatch)
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
            Affine (x1, Int (a * a'), x3, Int (a * b' + b)) | _ => raise UnexpectedMatch) (left_occ @- [Affine (x1, Int a, evar, Int b)])
      | Affine (x1, Rat a, evar, Rat b) => List.map (fn
          Affine (_, Rat a', x3, Rat b') =>
            Affine (x1, Rat ( */ (a, a')), x3, Rat (+/ ( */ (a, b'), b))) | _ => raise UnexpectedMatch) (left_occ @- [Affine (x1, Rat a, evar, Rat b)])
      | _ => raise UnexpectedMatch) right_occ)
  in
    eliminated @ no_occ
  end;

(* Affine variable elimination main step *)
fun schematic_elim_step (G: system) =
  let
    val affines        = (List.filter (fn cstr => case cstr of Affine _ => true | _ => false) G)
    val eliminable_constr =
      List.filter (fn
        Affine (x1 as Schematic _, a, x2, b) => List.exists (fn
          Affine (_, _, x2', _) =>
            x1 = x2' | _ => raise UnexpectedMatch) (affines @- [Affine(x1, a, x2, b)]) | _ => false) affines
    val eliminable_vars = List.map (fn Affine (x, _, _, _) => x | _ => raise UnexpectedMatch) eliminable_constr
  in
    if is_empty (eliminable_vars)
    then G
    else schematic_elim G (List.nth (eliminable_vars, 0))
  end;
fun all_schematic_elim (G: system) =
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

(* Constants propagation is possible whenever there exists: *)
fun constants_propagation_candidates_int (G: system) =
  let
    (* - Two timestamps [H \<Down>\<^sub>\<sigma> C] and [H \<Down>\<^sub>\<sigma> X], where [X] a variable and [C] is a constant (by injectivity) *)
    val timestamp_var_right = (List.filter (fn cstr => case cstr of Timestamp (_, _, Schematic _) => true | _ => false) G)
    val timestamp_cst_right = (List.filter (fn cstr => case cstr of Timestamp (_, _, Int _) => true | _ => false) G)
    val timestamp_unifiers = List.concat (List.concat (List.map (fn
      Timestamp (clk, k, Schematic (_, _)) => (List.map (fn
        Timestamp (clk', k', Int i) => (
          if clk = clk' andalso k = k'
          then [(Schematic (clk, k), Int i)]
          else []) | _ => raise UnexpectedMatch) timestamp_cst_right) | _ => raise UnexpectedMatch) timestamp_var_right))
    (* - Two timestamps [H \<Down>\<^sub>\<sigma> C] and [H' \<Down>\<^sub>\<sigma>\<^sub>' X\<^sup>\<sigma>\<^sub>H + _], where [X\<^sup>\<sigma>\<^sub>H] a variable and [C] is a constant *)
    val timestamp_add_schem_left = (List.filter (fn cstr => case cstr of Timestamp (_, _, Add (Schematic _, _)) => true | _ => false) G)
    val timestamp_add_schem_left_unifiers =
    List.concat (List.concat (List.map (fn
      Timestamp (_, _, Add (Schematic (clk1, k1), t)) =>
        (List.map (fn
        Timestamp (clk2, k2, Int i) => (
          if clk1 = clk2 andalso k1 = k2
          then [(Add (Schematic (clk1, k1), t), Add (Int i, t))]
          else []) | _ => raise UnexpectedMatch) timestamp_cst_right) | _ => raise UnexpectedMatch) timestamp_add_schem_left))
    (* - Two timestamps [H \<Down>\<^sub>\<sigma> C] and [H' \<Down>\<^sub>\<sigma>\<^sub>' _ + X\<^sup>\<sigma>\<^sub>H], where [X\<^sup>\<sigma>\<^sub>H] a variable and [C] is a constant *)
    val timestamp_add_schem_right = (List.filter (fn cstr => case cstr of Timestamp (_, _, Add (_, Schematic _)) => true | _ => false) G)
    val timestamp_add_schem_right_unifiers =
    List.concat (List.concat (List.map (fn
      Timestamp (_, _, Add (t, Schematic (clk1, k1))) =>
        (List.map (fn
        Timestamp (clk2, k2, Int i) => (
          if clk1 = clk2 andalso k1 = k2
          then [(Add (t, Schematic (clk1, k1)), Add (Int i, t))]
          else []) | _ => raise UnexpectedMatch) timestamp_cst_right) | _ => raise UnexpectedMatch) timestamp_add_schem_right))
    (* - Two timestamps [H \<Down>\<^sub>\<sigma> C\<^sub>1 + C\<^sub>2], where [C\<^sub>1] and [C\<^sub>2] are constants *)
    val timestamp_add_csts = (List.filter (fn cstr => case cstr of Timestamp (_, _, Add (Int _, Int _)) => true | _ => false) G)
    val timestamp_add_csts_unifiers = List.map
      (fn Timestamp (_, _, Add (Int n1, Int n2)) => (Add (Int n1, Int n2), Int (n1 + n2)) | _ => raise UnexpectedMatch)
      timestamp_add_csts
    (* - Affine relation [X = a * C + b] (by (+, \<times>) closure of Z) *)
    val affine_var_left_only = List.filter (fn cstr => case cstr of Affine (Schematic _, Int _, Int _, Int _) => true | _ => false) G
    val affine_cst_left_only = List.filter (fn cstr => case cstr of Affine (Int _, Int _, Schematic _, Int _) => true | _ => false) G
    val affine_left_unifiers =
      List.map
        (fn Affine (Schematic (clk, k), Int a, Int i, Int b) => (Schematic (clk, k), Int (a * i + b))  | _ => raise UnexpectedMatch)
        affine_var_left_only
    (* - Affine relation [C = a * X + b] if [X] has a solution in Z *)
    val affine_right_unifiers =
      List.concat (List.map (fn Affine (Int i, Int a, X as Schematic _, Int b) =>
        if (i - b) mod a = 0
        then [(X, Int ((i - b) div a))]
        else []
         | _ => raise UnexpectedMatch
        ) affine_cst_left_only)
    (* - Affine relation with integer fixpoint [X = a * X + b] if [X] has a solution in Z *)
    val affine_fixpoint_var = List.filter (fn cstr => case cstr of Affine (X1 as Schematic _, Int _, X2 as Schematic _, Int _) => X1 = X2 | _ => false) G
    val affine_fixpoint_unifiers =
      List.concat (List.map (fn Affine (X as Schematic _, Int a, Schematic _, Int b) =>
        if b mod (1 - a) = 0
        then [(X, Int (b div (1 - a)))]
        else []
         | _ => raise UnexpectedMatch
        ) affine_fixpoint_var)
    (* - Two timestamps [H \<Down>\<^sub>\<sigma> C] and [X\<^sup>\<sigma>\<^sub>H = \<alpha> * _ + \<beta>], where [X\<^sup>\<sigma>\<^sub>H] is a variable [C] is a constant *)
    val affine_var_left = List.filter (fn cstr => case cstr of Affine (Schematic _, Int _, _, Int _) => true | _ => false) G
    val affine_timestamp_left_var_unifiers =
    List.concat (List.concat (List.map (fn
      Affine (Schematic (clk1, k1), Int a, v, Int b) =>
        (List.map (fn
        Timestamp (clk2, k2, Int i) => (
          if clk1 = clk2 andalso k1 = k2
          then [(Schematic (clk1, k1), Int i)]
          else []) | _ => raise UnexpectedMatch) timestamp_cst_right) | _ => raise UnexpectedMatch) affine_var_left))
    (* - Two timestamps [H \<Down>\<^sub>\<sigma> C] and [_ = \<alpha> * X\<^sup>\<sigma>\<^sub>H + \<beta>], where [X\<^sup>\<sigma>\<^sub>H] is a variable [C] is a constant *)
    val affine_var_right = List.filter (fn cstr => case cstr of Affine (_, Int _, Schematic _, Int _) => true | _ => false) G
    val affine_timestamp_right_var_unifiers =
    List.concat (List.concat (List.map (fn
      Affine (v, Int a, Schematic (clk1, k1), Int b) =>
        (List.map (fn
        Timestamp (clk2, k2, Int i) => (
          if clk1 = clk2 andalso k1 = k2
          then [(Schematic (clk1, k1), Int i)]
          else []) | _ => raise UnexpectedMatch) timestamp_cst_right) | _ => raise UnexpectedMatch) affine_var_right))
  in
    timestamp_unifiers
    @ timestamp_add_schem_left_unifiers
    @ timestamp_add_schem_right_unifiers
    @ timestamp_add_csts_unifiers
    @ affine_left_unifiers
    @ affine_right_unifiers
    @ affine_fixpoint_unifiers
    @ affine_timestamp_left_var_unifiers
    @ affine_timestamp_right_var_unifiers
  end;

fun constants_propagation_candidates_rat (G: system) =
  let
    (* - Two timestamps [H \<Down>\<^sub>\<sigma> C] and [H \<Down>\<^sub>\<sigma> X], where [X] a variable and [C] is a constant (by injectivity) *)
    val timestamp_var_right = (List.filter (fn cstr => case cstr of Timestamp (_, _, Schematic _) => true | _ => false) G)
    val timestamp_cst_right = (List.filter (fn cstr => case cstr of Timestamp (_, _, Rat _) => true | _ => false) G)
    val timestamp_unifiers = List.concat (List.concat (List.map (fn
      Timestamp (clk, k, Schematic (_, _)) => (List.map (fn
        Timestamp (clk', k', Rat i) => (
          if clk = clk' andalso k = k'
          then [(Schematic (clk, k), Rat i)]
          else []) | _ => raise UnexpectedMatch) timestamp_cst_right) | _ => raise UnexpectedMatch) timestamp_var_right))
    (* - Two timestamps [H \<Down>\<^sub>\<sigma> C] and [H' \<Down>\<^sub>\<sigma>\<^sub>' X\<^sup>\<sigma>\<^sub>H + _], where [X\<^sup>\<sigma>\<^sub>H] a variable and [C] is a constant *)
    val timestamp_add_schem_left = (List.filter (fn cstr => case cstr of Timestamp (_, _, Add (Schematic _, _)) => true | _ => false) G)
    val timestamp_add_schem_left_unifiers =
    List.concat (List.concat (List.map (fn
      Timestamp (_, _, Add (Schematic (clk1, k1), t)) =>
        (List.map (fn
        Timestamp (clk2, k2, Rat i) => (
          if clk1 = clk2 andalso k1 = k2
          then [(Add (Schematic (clk1, k1), t), Add (Rat i, t))]
          else []) | _ => raise UnexpectedMatch) timestamp_cst_right) | _ => raise UnexpectedMatch) timestamp_add_schem_left))
    (* - Two timestamps [H \<Down>\<^sub>\<sigma> C] and [H' \<Down>\<^sub>\<sigma>\<^sub>' _ + X\<^sup>\<sigma>\<^sub>H], where [X\<^sup>\<sigma>\<^sub>H] a variable and [C] is a constant *)
    val timestamp_add_schem_right = (List.filter (fn cstr => case cstr of Timestamp (_, _, Add (_, Schematic _)) => true | _ => false) G)
    val timestamp_add_schem_right_unifiers =
    List.concat (List.concat (List.map (fn
      Timestamp (_, _, Add (t, Schematic (clk1, k1))) =>
        (List.map (fn
        Timestamp (clk2, k2, Rat i) => (
          if clk1 = clk2 andalso k1 = k2
          then [(Add (t, Schematic (clk1, k1)), Add (Rat i, t))]
          else []) | _ => raise UnexpectedMatch) timestamp_cst_right) | _ => raise UnexpectedMatch) timestamp_add_schem_right))
    (* - Two timestamps [H \<Down>\<^sub>\<sigma> C\<^sub>1 + C\<^sub>2], where [C\<^sub>1] and [C\<^sub>2] are constants *)
    val timestamp_add_csts = (List.filter (fn cstr => case cstr of Timestamp (_, _, Add (Rat _, Rat _)) => true | _ => false) G)
    val timestamp_add_csts_unifiers = List.map
      (fn Timestamp (_, _, Add (Rat x1, Rat x2)) => (Add (Rat x1, Rat x2), Rat (+/ (x1, x2))) | _ => raise UnexpectedMatch)
      timestamp_add_csts
    (* - Affine relation [X = a * C + b] (by (+, \<times>) closure of \<rat> ) *)
    val affine_var_left_only = List.filter (fn cstr => case cstr of Affine (Schematic _, Rat _, Rat _, Rat _) => true | _ => false) G
    val affine_left_unifiers =
      List.map
        (fn Affine (Schematic (clk, k), Rat a, Rat x, Rat b) => (Schematic (clk, k), Rat (+/ ( */ (a, x), b)))  | _ => raise UnexpectedMatch)
        affine_var_left_only
    (* - Affine relation [C = a * X + b] if [X] has a solution in \<rat> *)
    val affine_cst_left_only = List.filter (fn cstr => case cstr of Affine (Rat _, Rat _, Schematic _, Rat _) => true | _ => false) G
    val affine_right_unifiers =
      List.concat (List.map (fn Affine (Rat r, Rat a, X as Schematic _, Rat b) =>
        if <>/ (a, rat_zero)
        then [(X, Rat (// (-/ (r, b), a)))]
        else []
         | _ => raise UnexpectedMatch
        ) affine_cst_left_only)
    (* - Affine relation with integer fixpoint [X = a * X + b] if [X] has a solution in \<rat> *)
    val affine_fixpoint_var = List.filter (fn cstr => case cstr of Affine (X1 as Schematic _, Rat _, X2 as Schematic _, Rat _) => X1 = X2 | _ => false) G
    val affine_fixpoint_unifiers =
      List.concat (List.map (fn Affine (X as Schematic _, Rat a, Schematic _, Rat b) =>
        if <>/ (-/ (rat_one, a), rat_zero)
        then [(X, Rat (// (b, -/ (rat_one, a))))]
        else []
         | _ => raise UnexpectedMatch
        ) affine_fixpoint_var)
    (* - Two timestamps [H \<Down>\<^sub>\<sigma> C] and [X\<^sup>\<sigma>\<^sub>H = \<alpha> * _ + \<beta>], where [X\<^sup>\<sigma>\<^sub>H] is a variable [C] is a constant *)
    val affine_var_left = List.filter (fn cstr => case cstr of Affine (Schematic _, Rat _, _, Rat _) => true | _ => false) G
    val affine_timestamp_left_var_unifiers =
    List.concat (List.concat (List.map (fn
      Affine (Schematic (clk1, k1), Rat a, v, Rat b) =>
        (List.map (fn
        Timestamp (clk2, k2, Rat x) => (
          if clk1 = clk2 andalso k1 = k2
          then [(Schematic (clk1, k1), Rat x)]
          else []) | _ => raise UnexpectedMatch) timestamp_cst_right) | _ => raise UnexpectedMatch) affine_var_left))
    (* - Two timestamps [H \<Down>\<^sub>\<sigma> C] and [_ = \<alpha> * X\<^sup>\<sigma>\<^sub>H + \<beta>], where [X\<^sup>\<sigma>\<^sub>H] is a variable [C] is a constant *)
    val affine_var_right = List.filter (fn cstr => case cstr of Affine (_, Rat _, Schematic _, Rat _) => true | _ => false) G
    val affine_timestamp_right_var_unifiers =
    List.concat (List.concat (List.map (fn
      Affine (v, Rat a, Schematic (clk1, k1), Rat b) =>
        (List.map (fn
        Timestamp (clk2, k2, Rat i) => (
          if clk1 = clk2 andalso k1 = k2
          then [(Schematic (clk1, k1), Rat i)]
          else []) | _ => raise UnexpectedMatch) timestamp_cst_right) | _ => raise UnexpectedMatch) affine_var_right))
  in
    timestamp_unifiers
    @ timestamp_add_schem_left_unifiers
    @ timestamp_add_schem_right_unifiers
    @ timestamp_add_csts_unifiers
    @ affine_left_unifiers
    @ affine_right_unifiers
    @ affine_fixpoint_unifiers
    @ affine_timestamp_left_var_unifiers
    @ affine_timestamp_right_var_unifiers
  end;

(* Constant propagation main step *)
fun constants_propagation_step_int (G: system) =
  List.foldl (fn (subst, h) => apply_tag_substition subst h) G (constants_propagation_candidates_int G);
fun constants_propagation_step_rat (G: system) =
  List.foldl (fn (subst, h) => apply_tag_substition subst h) G (constants_propagation_candidates_rat G);
fun constants_propagation (G: system) =
  lfp (constants_propagation_step_rat o constants_propagation_step_int) G

(* Remove trivial schematic timestamp of the kind [H \<Down>\<^sub>n X\<^sup>H\<^sub>n]*)
fun no_trivial_schem_timestamp (G: system) =
  List.filter (fn Timestamp (c, n, Schematic (c', n')) => not (c = c' andalso n = n') | _ => true) G

(* Affine equations with (only) constants elimination *)
fun constant_affine_eqns_elim (G: system) : system =
  List.filter (fn
      Affine (Int t1, Int a, Int t2, Int b) => t1 <> a * t2 + b
    | Affine (Rat t1, Rat a, Rat t2, Rat b) => <>/ (t1, +/ ( */ (a, t2), b))
    | _ => true) G

(* Equation [X = a * X + b] has a solution:
   - in Z iff [b] is dividable by [(1 - a)]
   - in \<rat> iff [(1 - a)] \<noteq> 0
*)
fun check_fixpoint_affeqns (G: system) : bool =
  List.all (fn
    Affine (X1 as Schematic _, Int a, X2 as Schematic _, Int b) => not (X1 = X2) orelse (b mod (1 - a)) = 0
  | Affine (X1 as Schematic _, Rat a, X2 as Schematic _, Rat b) => not (X1 = X2) orelse <>/ (-/ (rat_one, a), rat_zero)
  | _ => true) G

(* Equation with constants only [C1 = a * C2 + b] *)
fun check_constants_affeqns (G: system) : bool =
  List.all (fn
    Affine (Int c1, Int a, Int c2, Int b) => c1 = a * c2 + b
  | Affine (Rat c1, Rat a, Rat c2, Rat b) => =/ (c1, +/ ( */ (a, c2), b))
  | _ => true) G

(* An equation [C = a * X + b] has a solution
   - in Z iff [C - a] is dividable by [b]
   - in \<rat> iff [a] \<noteq> 0
*)
fun check_varright_affeqns (G: system) : bool =
  List.all (fn
    Affine (Int C, Int a, Schematic _, Int b) => (C - b) mod a = 0
  | Affine (Rat C, Rat a, Schematic _, Rat b) => <>/ (a, rat_zero)
  | _ => true) G

(* For equations of the general kind [X1 = a * X2 + b], all of them are together satisfiable
   when they all share no common variables *)
fun check_no_shared_var_affeqns (G: system) =
  let 
    val affine_var_var = (List.filter (fn cstr => case cstr of Affine (Schematic _, _, Schematic _, _) => true | _ => false) G)
    val vars_in_affine_var_var = List.concat (List.map (fn Affine (v1 as Schematic _, _, v2 as Schematic _, _) => [v1, v2] | _ => []) affine_var_var)
    val vars_cardinalities = List.map (fn v => (v, List.length (List.filter (fn v' => v = v') vars_in_affine_var_var))) vars_in_affine_var_var
  in List.all (fn (_, n) => n <= 1) vars_cardinalities
  end;

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
  (* UNSAFE: Please investigate *)
  (* andalso check_no_shared_var_affeqns G *)
  andalso check_varright_affeqns G;

fun reduce (G: system) =
  no_trivial_schem_timestamp (constant_affine_eqns_elim (all_schematic_elim (constants_propagation (uniq G))))

fun SAT (G: system) : bool =
  let val G_prop_and_elim_until_fp = lfp (reduce) G (* Keep reducing *)
  in decide G_prop_and_elim_until_fp                 (* Then decide! *)
  end

fun context_SAT ((G, _, _, _) : TESL_ARS_conf) =
  SAT G
