(**
   Module ArithmeticSolver

   Author : Hai Nguyen Van
            LRI, Université Paris-Sud/CNRS
   
   The copyright to this code is held by Laboratoire de Recherche en
   Informatique, Université Paris-Sud/CNRS. All rights reserved. This
   file is distributed under the MIT License.
*)

(* Constraint solver for the ARS used in reducing TESL *)
(* The logic structure contains
   1. Booleans fixed-predicates [C ⇑_σ] and [C \<not>⇑_σ]
   2. Affine relations
     - Over integers (ℤ, +, ×)
     - With constants [C_1], [C_2],... and variables [V_1], [V_2], ...
     - Of the kind [X_1 = α × X_2 + β], where [X_1] and [X_2] can be constants or variables
   3. Tag assignments [C ⇓_σ τ] where [τ] is a constant
   4. Conjunction
*)

exception UnexpectedMatch_RunConsistency_1
exception UnexpectedMatch_RunConsistency_2
exception UnexpectedMatch_RunConsistency_3
exception UnexpectedMatch_RunConsistency_4
exception UnexpectedMatch_RunConsistency_5
exception UnexpectedMatch_RunConsistency_6
exception UnexpectedMatch_RunConsistency_7
exception UnexpectedMatch_RunConsistency_8
exception UnexpectedMatch_RunConsistency_9
exception UnexpectedMatch_RunConsistency_10
exception UnexpectedMatch_RunConsistency_11
exception UnexpectedMatch_RunConsistency_12
exception UnexpectedMatch_RunConsistency_13
exception UnexpectedMatch_RunConsistency_14
exception UnexpectedMatch_RunConsistency_15
exception UnexpectedMatch_RunConsistency_16
exception UnexpectedMatch_RunConsistency_17
exception UnexpectedMatch_RunConsistency_18
exception UnexpectedMatch_RunConsistency_19
exception UnexpectedMatch_RunConsistency_20
exception UnexpectedMatch_RunConsistency_21
exception UnexpectedMatch_RunConsistency_22
exception UnexpectedMatch_RunConsistency_23
exception UnexpectedMatch_RunConsistency_24
exception UnexpectedMatch_RunConsistency_25
exception UnexpectedMatch_RunConsistency_26
exception UnexpectedMatch_RunConsistency_27
exception UnexpectedMatch_RunConsistency_28
exception UnexpectedMatch_RunConsistency_29
exception UnexpectedMatch_RunConsistency_30
exception UnexpectedMatch_RunConsistency_31
exception UnexpectedMatch_RunConsistency_32
exception UnexpectedMatch_RunConsistency_33
exception UnexpectedMatch_RunConsistency_34
exception UnexpectedMatch_RunConsistency_35
exception UnexpectedMatch_RunConsistency_36
exception UnexpectedMatch_RunConsistency_37
exception UnexpectedMatch_RunConsistency_38
exception UnexpectedMatch_RunConsistency_39
exception UnexpectedMatch_RunConsistency_40
exception UnexpectedMatch_RunConsistency_41
exception UnexpectedMatch_RunConsistency_42
exception UnexpectedMatch_RunConsistency_43
exception UnexpectedMatch_RunConsistency_44
exception UnexpectedMatch_RunConsistency_45
exception UnexpectedMatch_RunConsistency_46
exception UnexpectedMatch_RunConsistency_47
exception UnexpectedMatch_RunConsistency_48
exception UnexpectedMatch_RunConsistency_49
exception UnexpectedMatch_RunConsistency_50
exception UnexpectedMatch_RunConsistency_51
exception UnexpectedMatch_RunConsistency_52
exception UnexpectedMatch_RunConsistency_53
exception UnexpectedMatch_RunConsistency_54
exception UnexpectedMatch_RunConsistency_55
exception UnexpectedMatch_RunConsistency_56
exception UnexpectedMatch_RunConsistency_57
exception UnexpectedMatch_RunConsistency_58
exception UnexpectedMatch_RunConsistency_59
exception UnexpectedMatch_RunConsistency_60
exception UnexpectedMatch_RunConsistency_61
exception UnexpectedMatch_RunConsistency_62
exception UnexpectedMatch_RunConsistency_63
exception UnexpectedMatch_RunConsistency_64
exception UnexpectedMatch_RunConsistency_65
exception UnexpectedMatch_RunConsistency_66
exception UnexpectedMatch_RunConsistency_67
exception UnexpectedMatch_RunConsistency_68
exception UnexpectedMatch_RunConsistency_69

(* Returns primitives for a given specific step *)
fun haa_constrs_at_step (G: system) (step: int) =
  List.filter (fn
      Timestamp (_, step', _) => step = step'
    | Ticks (_, step')        => step = step'
    | NotTicks (_, step')     => step = step'
    | NotTicksUntil (_, step')     => step = step'
    | NotTicksFrom (_, step')     => step = step'
    | Affine _                => false
    | AffineRefl _            => false
    | FunRel _                => false
) G

(* Whenever there's a ticking predicate, there shouln't be a refutation. Same way for non-ticking predicate. *)
fun check_non_contradictory_ticks (G: system) =
  let
    val ticks      = (List.filter (fn cstr => case cstr of Ticks _ => true | _ => false) G)
    val notticks   = (List.filter (fn cstr => case cstr of NotTicks _ => true | _ => false) G)
    val notticksuntil = (List.filter (fn cstr => case cstr of NotTicksUntil _ => true | _ => false) G)
    val notticksfrom  = (List.filter (fn cstr => case cstr of NotTicksFrom _ => true | _ => false) G)
  in
    List.all
      (fn Ticks (clk, i) => not (List.exists (fn NotTicks (clk', i') => clk = clk' andalso i = i' | _ => raise UnexpectedMatch_RunConsistency_1) notticks)
      	  	      	    andalso (not (List.exists (fn NotTicksUntil (clk', i') => clk = clk' andalso i < i' | _ => raise UnexpectedMatch_RunConsistency_2) notticksuntil))
      	  	      	    andalso (not (List.exists (fn NotTicksFrom  (clk', i') => clk = clk' andalso i >= i' | _ => raise UnexpectedMatch_RunConsistency_3) notticksfrom))
      	  | _ => raise UnexpectedMatch_RunConsistency_4)
      ticks
  andalso
    List.all
      (fn NotTicks (clk, i) => not (List.exists (fn Ticks (clk', i') => clk = clk' andalso i = i' | _ => raise UnexpectedMatch_RunConsistency_5) ticks) | _ => raise UnexpectedMatch_RunConsistency_6)
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
      (fn Timestamp (clk, i, tag) => List.all (fn Timestamp (clk', i', tag') => not (clk = clk' andalso i = i') orelse tag = tag' | _ => raise UnexpectedMatch_RunConsistency_7) timestamps | _ => raise UnexpectedMatch_RunConsistency_8)
      timestamps
  end;

(* Same as above but for variables. Two timestamps [H ⇓_σ V] and [H ⇓_σ V + \<delta>], where [\<delta> ≠ 0] *)
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
        | _ => raise UnexpectedMatch_RunConsistency_9) timestamps_add | _ => raise UnexpectedMatch_RunConsistency_10)
      timestamps_var
  end;

(* Whenever there's a timestamp constraint at fixed clock, all others
 *  with higher instant index also have higher time tags
 * ... except for clock declared as quantities
 *)
fun check_ascending_chain_on_timestamps (declared_quantities: clock list) (G: system) =
  let
    fun not_in e l = List.all (fn e' => e <> e) l
    val timestamps = (List.filter (fn cstr => case cstr of
        Timestamp (_, _, Unit)  => true
      | Timestamp (clk, _, Int _) => not_in clk declared_quantities
      | Timestamp (clk, _, Rat _) => not_in clk declared_quantities
      | _           => false) G)
  in List.all
      (fn Timestamp (clk, i, tag) => List.all (fn Timestamp (clk', i', tag') => not (clk = clk' andalso i < i') orelse ::<= (tag, tag') | _ => raise UnexpectedMatch_RunConsistency_11) timestamps | _ => raise UnexpectedMatch_RunConsistency_12)
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
          List.all (fn Timestamp (clk', _, tag') => not (clk = clk') orelse ::~ (tag, tag') | _ => raise UnexpectedMatch_RunConsistency_13) timestamps | _ => raise UnexpectedMatch_RunConsistency_14)
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
            Affine (x1, Int (a * a'), x3, Int (a * b' + b))
        | _ => raise UnexpectedMatch_RunConsistency_15) (left_occ @- [Affine (x1, Int a, evar, Int b)])
      | Affine (x1, Rat a, evar, Rat b) => List.map (fn
          Affine (_, Rat a', x3, Rat b') =>
            Affine (x1, Rat ( */ (a, a')), x3, Rat (+/ ( */ (a, b'), b)))
        | aff => aff (* TWEAK, check for well-foundedness *)
        | _ => raise UnexpectedMatch_RunConsistency_16
        ) (left_occ @- [Affine (x1, Rat a, evar, Rat b)])
      | aff => [aff] (* TWEAK, check for well-foundedness *)
      | _ => raise UnexpectedMatch_RunConsistency_17) right_occ)
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
            x1 = x2' | _ => raise UnexpectedMatch_RunConsistency_18) (affines @- [Affine(x1, a, x2, b)]) | _ => false) affines
    val eliminable_vars = List.map (fn Affine (x, _, _, _) => x | _ => raise UnexpectedMatch_RunConsistency_19) eliminable_constr
  in
    if is_empty (eliminable_vars)
    then G
    else schematic_elim G (List.nth (eliminable_vars, 0))
  end;
fun all_schematic_elim (G: system) =
  lfp (schematic_elim_step) G

(* Apply a complete substitution where tag [t_old] is substituted by tag [t_new] in constraint system [G] *)
fun apply_tag_substition ((t_old, t_new): tag * tag) (G: system) =
  List.map (fn 
      Timestamp (clk, k, t)     => if t = t_old then Timestamp (clk, k, t_new) else Timestamp (clk, k, t)
    | Affine (ltag, a, rtag, b) =>
      if ltag = t_old andalso rtag = t_old
      then Affine (t_new, a, t_new, b)
      else if ltag = t_old
        then Affine (t_new, a, rtag, b)
        else if rtag = t_old
          then Affine (ltag, a, t_new, b)
          else if a = t_old
            then Affine (ltag, t_new, rtag, b)
            else if b = t_old
              then Affine (ltag, a, rtag, t_new)
              else Affine (ltag, a, rtag, b)
    | AffineRefl (ltag, rtag) =>
(*
      if ltag = t_old andalso rtag = t_old
      then AffineRefl (t_new, t_new)
      else
*)
      if ltag = t_old
      then AffineRefl (t_new, rtag)
      else
	 if rtag = t_old
	 then AffineRefl (ltag, t_new)
        else AffineRefl (ltag, rtag)
    | x => x) G;

(* Constants propagation is possible whenever there exists: *)
(* TODO: Deperecated, needs to be updated with latest changes *)
fun constants_propagation_candidates_int (G: system) =
  let
    (* - Two timestamps [H ⇓_σ C] and [H ⇓_σ X], where [X] a variable and [C] is a constant (by injectivity) *)
    val timestamp_var_right = (List.filter (fn cstr => case cstr of Timestamp (_, _, Schematic _) => true | _ => false) G)
    val timestamp_cst_right = (List.filter (fn cstr => case cstr of Timestamp (_, _, Int _) => true | _ => false) G)
    val timestamp_unifiers = List.concat (List.concat (List.map (fn
      Timestamp (clk, k, Schematic (_, _)) => (List.map (fn
        Timestamp (clk', k', Int i) => (
          if clk = clk' andalso k = k'
          then [(Schematic (clk, k), Int i)]
          else []) | _ => raise UnexpectedMatch_RunConsistency_20) timestamp_cst_right) | _ => raise UnexpectedMatch_RunConsistency_21) timestamp_var_right))
    (* - Two timestamps [H ⇓_σ C] and [H' ⇓_σ' X^σ_H + _], where [X^σ_H] a variable and [C] is a constant *)
    val timestamp_add_schem_left = (List.filter (fn cstr => case cstr of Timestamp (_, _, Add (Schematic _, _)) => true | _ => false) G)
    val timestamp_add_schem_left_unifiers =
    List.concat (List.concat (List.map (fn
      Timestamp (_, _, Add (Schematic (clk1, k1), t)) =>
        (List.map (fn
        Timestamp (clk2, k2, Int i) => (
          if clk1 = clk2 andalso k1 = k2
          then [(Add (Schematic (clk1, k1), t), Add (Int i, t))]
          else []) | _ => raise UnexpectedMatch_RunConsistency_22) timestamp_cst_right) | _ => raise UnexpectedMatch_RunConsistency_23) timestamp_add_schem_left))
    (* - Two timestamps [H ⇓_σ C] and [H' ⇓_σ' _ + X^σ_H], where [X^σ_H] a variable and [C] is a constant *)
    val timestamp_add_schem_right = (List.filter (fn cstr => case cstr of Timestamp (_, _, Add (_, Schematic _)) => true | _ => false) G)
    val timestamp_add_schem_right_unifiers =
    List.concat (List.concat (List.map (fn
      Timestamp (_, _, Add (t, Schematic (clk1, k1))) =>
        (List.map (fn
        Timestamp (clk2, k2, Int i) => (
          if clk1 = clk2 andalso k1 = k2
          then [(Add (t, Schematic (clk1, k1)), Add (Int i, t))]
          else []) | _ => raise UnexpectedMatch_RunConsistency_24) timestamp_cst_right) | _ => raise UnexpectedMatch_RunConsistency_25) timestamp_add_schem_right))
    (* - Two timestamps [H ⇓_σ C_1 + C_2], where [C_1] and [C_2] are constants *)
    val timestamp_add_csts = (List.filter (fn cstr => case cstr of Timestamp (_, _, Add (Int _, Int _)) => true | _ => false) G)
    val timestamp_add_csts_unifiers = List.map
      (fn Timestamp (_, _, Add (Int n1, Int n2)) => (Add (Int n1, Int n2), Int (n1 + n2)) | _ => raise UnexpectedMatch_RunConsistency_26)
      timestamp_add_csts
    (* - Affine relation [X = a * C + b] (by (+, ×) closure of Z) *)
    val affine_var_left_only = List.filter (fn cstr => case cstr of Affine (Schematic _, Int _, Int _, Int _) => true | _ => false) G
    val affine_cst_left_only = List.filter (fn cstr => case cstr of Affine (Int _, Int _, Schematic _, Int _) => true | _ => false) G
    val affine_left_unifiers =
      List.map
        (fn Affine (Schematic (clk, k), Int a, Int i, Int b) => (Schematic (clk, k), Int (a * i + b))  | _ => raise UnexpectedMatch_RunConsistency_27)
        affine_var_left_only
    (* - Affine relation [C = a * X + b] if [X] has a solution in Z *)
    val affine_right_unifiers =
      List.concat (List.map (fn Affine (Int i, Int a, X as Schematic _, Int b) =>
        if (i - b) mod a = 0
        then [(X, Int ((i - b) div a))]
        else []
         | _ => raise UnexpectedMatch_RunConsistency_28
        ) affine_cst_left_only)
    (* - Affine relation with integer fixpoint [X = a * X + b] if [X] has a solution in Z *)
    val affine_fixpoint_var = List.filter (fn cstr => case cstr of Affine (X1 as Schematic _, Int _, X2 as Schematic _, Int _) => X1 = X2 | _ => false) G
    val affine_fixpoint_unifiers =
      List.concat (List.map (fn Affine (X as Schematic _, Int a, Schematic _, Int b) =>
        if b mod (1 - a) = 0
        then [(X, Int (b div (1 - a)))]
        else []
         | _ => raise UnexpectedMatch_RunConsistency_29
        ) affine_fixpoint_var)
    (* - Two timestamps [H ⇓_σ C] and [X^σ_H = _ * _ + _], where [X^σ_H] is a variable [C] is a constant *)
    val affine_var_1 = List.filter (fn cstr => case cstr of Affine (Schematic _, _, _, _) => true | _ => false) G
    val affine_timestamp_1_var_unifiers =
    List.concat (List.concat (List.map (fn
      Affine (Schematic (clk1, k1), _, v, _) =>
        (List.map (fn
        Timestamp (clk2, k2, Int x) => (
          if clk1 = clk2 andalso k1 = k2
          then [(Schematic (clk1, k1), Int x)]
          else []) | _ => raise UnexpectedMatch_RunConsistency_30) timestamp_cst_right) | _ => raise UnexpectedMatch_RunConsistency_31) affine_var_1))
    (* - Two timestamps [H ⇓_σ C] and [_ = X^σ_H * _ + _], where [X^σ_H] is a variable [C] is a constant *)
    val affine_var_2 = List.filter (fn cstr => case cstr of Affine (_, Schematic _, _, _) => true | _ => false) G
    val affine_timestamp_2_var_unifiers =
    List.concat (List.concat (List.map (fn
      Affine (v, Schematic (clk1, k1), _, _) =>
        (List.map (fn
        Timestamp (clk2, k2, Int i) => (
          if clk1 = clk2 andalso k1 = k2
          then [(Schematic (clk1, k1), Int i)]
          else []) | _ => raise UnexpectedMatch_RunConsistency_32) timestamp_cst_right) | _ => raise UnexpectedMatch_RunConsistency_33) affine_var_2))
    (* - Two timestamps [H ⇓_σ C] and [_ = _ * X^σ_H + _], where [X^σ_H] is a variable [C] is a constant *)
    val affine_var_3 = List.filter (fn cstr => case cstr of Affine (_, _, Schematic _, _) => true | _ => false) G
    val affine_timestamp_3_var_unifiers =
    List.concat (List.concat (List.map (fn
      Affine (v, _, Schematic (clk1, k1), _) =>
        (List.map (fn
        Timestamp (clk2, k2, Int i) => (
          if clk1 = clk2 andalso k1 = k2
          then [(Schematic (clk1, k1), Int i)]
          else []) | _ => raise UnexpectedMatch_RunConsistency_34) timestamp_cst_right) | _ => raise UnexpectedMatch_RunConsistency_35) affine_var_3))
    (* - Two timestamps [H ⇓_σ C] and [_ = _ * _ + X^σ_H], where [X^σ_H] is a variable [C] is a constant *)
    val affine_var_4 = List.filter (fn cstr => case cstr of Affine (_, _, _, Schematic _) => true | _ => false) G
    val affine_timestamp_4_var_unifiers =
    List.concat (List.concat (List.map (fn
      Affine (v, _, _, Schematic (clk1, k1)) =>
        (List.map (fn
        Timestamp (clk2, k2, Int i) => (
          if clk1 = clk2 andalso k1 = k2
          then [(Schematic (clk1, k1), Int i)]
          else []) | _ => raise UnexpectedMatch_RunConsistency_36) timestamp_cst_right) | _ => raise UnexpectedMatch_RunConsistency_37) affine_var_4))
    (* - Two timestamps [H ⇓_σ C] and [X^σ_H = _], where [X^σ_H] is a variable [C] is a constant *)
    val affine_var_refl_1 = List.filter (fn cstr => case cstr of AffineRefl (Schematic _, _) => true | _ => false) G
    val affine_timestamp_refl_1_var_unifiers =
    List.concat (List.concat (List.map (fn
      AffineRefl (Schematic (clk1, k1), _) =>
        (List.map (fn
        Timestamp (clk2, k2, Int i) => (
          if clk1 = clk2 andalso k1 = k2
          then [(Schematic (clk1, k1), Int i)]
          else []) | _ => raise UnexpectedMatch_RunConsistency_38) timestamp_cst_right) | _ => raise UnexpectedMatch_RunConsistency_39) affine_var_refl_1))
    (* - Two timestamps [H ⇓_σ C] and [_ = X^σ_H], where [X^σ_H] is a variable [C] is a constant *)
    val affine_var_refl_2 = List.filter (fn cstr => case cstr of AffineRefl (_, Schematic _) => true | _ => false) G
    val affine_timestamp_refl_2_var_unifiers =
    List.concat (List.concat (List.map (fn
      AffineRefl (_, Schematic (clk1, k1)) =>
        (List.map (fn
        Timestamp (clk2, k2, Int i) => (
          if clk1 = clk2 andalso k1 = k2
          then [(Schematic (clk1, k1), Int i)]
          else []) | _ => raise UnexpectedMatch_RunConsistency_40) timestamp_cst_right) | _ => raise UnexpectedMatch_RunConsistency_41) affine_var_refl_2))
  in
    timestamp_unifiers
    @ timestamp_add_schem_left_unifiers
    @ timestamp_add_schem_right_unifiers
    @ timestamp_add_csts_unifiers
    @ affine_left_unifiers
    @ affine_right_unifiers
    @ affine_fixpoint_unifiers
    @ affine_timestamp_1_var_unifiers
    @ affine_timestamp_2_var_unifiers
    @ affine_timestamp_3_var_unifiers
    @ affine_timestamp_4_var_unifiers
    @ affine_timestamp_refl_1_var_unifiers
    @ affine_timestamp_refl_2_var_unifiers
  end;

exception UnknownFunctionSymbolOrIncorrectArity
exception NotYetConcretizedforFunctionInterpr
fun interpret_fun G fname schem_list : rat =
    let val concretized_list =
	     List.map (fn Schematic (clk, k) => (case (List.find (fn Timestamp (clk', k', Rat_) => clk = clk' andalso k = k' | _ => false) G) of
							   SOME (Timestamp (_, _, Rat r)) => r
							 | _ => raise NotYetConcretizedforFunctionInterpr)
			| _ => raise UnexpectedMatch_RunConsistency_64) schem_list
    in case (fname, concretized_list) of
	   ("pi", [])             => rat_of_real (Math.pi)
 	 | ("e", [])              => rat_of_real (Math.e)
	 | ("sqrt", x :: _)       => rat_of_real (Math.sqrt (real_of_rat x))
 	 | ("sin", x :: _)        => rat_of_real (Math.sin (real_of_rat x))
 	 | ("cos", x :: _)        => rat_of_real (Math.cos (real_of_rat x))
 	 | ("tan", x :: _)        => rat_of_real (Math.tan (real_of_rat x))
 	 | ("asin", x :: _)       => rat_of_real (Math.asin (real_of_rat x))
 	 | ("acos", x :: _)       => rat_of_real (Math.acos (real_of_rat x))
 	 | ("atan", x :: _)       => rat_of_real (Math.atan (real_of_rat x))
 	 | ("atan2", x :: y :: _) => rat_of_real (Math.atan2 (real_of_rat x, real_of_rat y))
 	 | ("exp", x :: _)        => rat_of_real (Math.exp (real_of_rat x))
 	 | ("pow", x :: y :: _)   => rat_of_real (Math.pow (real_of_rat x, real_of_rat y))
 	 | ("ln", x :: _)         => rat_of_real (Math.ln (real_of_rat x))
 	 | ("log10", x :: _)      => rat_of_real (Math.log10 (real_of_rat x))
 	 | ("sinh", x :: _)       => rat_of_real (Math.sinh (real_of_rat x))
 	 | ("cosh", x :: _)       => rat_of_real (Math.cosh (real_of_rat x))
 	 | ("tanh", x :: _)       => rat_of_real (Math.tanh (real_of_rat x))
	 | _                      => raise UnknownFunctionSymbolOrIncorrectArity
    end
fun is_interpretable G fname schem_list =
    let val _ = interpret_fun G fname schem_list
    in true
    end
    handle UnknownFunctionSymbolOrIncorrectArity => false
	  | NotYetConcretizedforFunctionInterpr => false

fun constants_propagation_candidates_rat (G: system) =
  let
    (**  Rearranged similar equation cases **)
    (* - Affine relation [C = X * a + b] --> [C = a * X + b] where [X] is a variable *)
    val G = List.map (fn Affine (Rat r1, Schematic s, Rat r2, Rat r3) => Affine (Rat r1, Rat r2, Schematic s, Rat r3) | g => g) G
    (* - Affine relation [C = a * a' + X] --> [C = 1 * X + (a * a')] where [X] is a variable *)
    val G = List.map (fn Affine (Rat c, Rat a, Rat a', Schematic x) => Affine (Rat c, Rat rat_one, Schematic x, Rat ( */ (a, a'))) | g => g) G
    (* - Two timestamps [H ⇓_σ C] and [H ⇓_σ X], where [X] a variable and [C] is a constant (by injectivity) *)
    val timestamp_var_right = (List.filter (fn cstr => case cstr of Timestamp (_, _, Schematic _) => true | _ => false) G)
    val timestamp_cst_right = (List.filter (fn cstr => case cstr of Timestamp (_, _, Rat _) => true | _ => false) G)
    val timestamp_unifiers = List.concat (List.concat (List.map (fn
      Timestamp (clk, k, Schematic (_, _)) => (List.map (fn
        Timestamp (clk', k', Rat i) => (
          if clk = clk' andalso k = k'
          then [(Schematic (clk, k), Rat i)]
          else []) | _ => raise UnexpectedMatch_RunConsistency_42) timestamp_cst_right) | _ => raise UnexpectedMatch_RunConsistency_43) timestamp_var_right))
    (* - Two timestamps [H ⇓_σ C] and [H' ⇓_σ' X^σ_H + _], where [X^σ_H] a variable and [C] is a constant *)
    val timestamp_add_schem_left = (List.filter (fn cstr => case cstr of Timestamp (_, _, Add (Schematic _, _)) => true | _ => false) G)
    val timestamp_add_schem_left_unifiers =
    List.concat (List.concat (List.map (fn
      Timestamp (_, _, Add (Schematic (clk1, k1), t)) =>
        (List.map (fn
        Timestamp (clk2, k2, Rat i) => (
          if clk1 = clk2 andalso k1 = k2
          then [(Add (Schematic (clk1, k1), t), Add (Rat i, t))]
          else []) | _ => raise UnexpectedMatch_RunConsistency_44) timestamp_cst_right) | _ => raise UnexpectedMatch_RunConsistency_45) timestamp_add_schem_left))
    (* - Two timestamps [H ⇓_σ C] and [H' ⇓_σ' _ + X^σ_H], where [X^σ_H] a variable and [C] is a constant *)
    val timestamp_add_schem_right = (List.filter (fn cstr => case cstr of Timestamp (_, _, Add (_, Schematic _)) => true | _ => false) G)
    val timestamp_add_schem_right_unifiers =
    List.concat (List.concat (List.map (fn
      Timestamp (_, _, Add (t, Schematic (clk1, k1))) =>
        (List.map (fn
        Timestamp (clk2, k2, Rat i) => (
          if clk1 = clk2 andalso k1 = k2
          then [(Add (t, Schematic (clk1, k1)), Add (Rat i, t))]
          else []) | _ => raise UnexpectedMatch_RunConsistency_46) timestamp_cst_right) | _ => raise UnexpectedMatch_RunConsistency_47) timestamp_add_schem_right))
    (* - Two timestamps [H ⇓_σ C_1 + C_2], where [C_1] and [C_2] are constants *)
    val timestamp_add_csts = (List.filter (fn cstr => case cstr of Timestamp (_, _, Add (Rat _, Rat _)) => true | _ => false) G)
    val timestamp_add_csts_unifiers = List.map
      (fn Timestamp (_, _, Add (Rat x1, Rat x2)) => (Add (Rat x1, Rat x2), Rat (+/ (x1, x2))) | _ => raise UnexpectedMatch_RunConsistency_48)
      timestamp_add_csts
    (* - Affine relation [X = a * C + b] (by (+, ×) closure of ℚ ) *)
    val affine_var_left_only = List.filter (fn cstr => case cstr of Affine (Schematic _, Rat _, Rat _, Rat _) => true | _ => false) G
    val affine_left_unifiers =
      List.map
        (fn Affine (Schematic (clk, k), Rat a, Rat x, Rat b) => (Schematic (clk, k), Rat (+/ ( */ (a, x), b)))  | _ => raise UnexpectedMatch_RunConsistency_49)
        affine_var_left_only
    (* - Affine relation [C = a * X + b] if [X] has a solution in ℚ *)
    val affine_cst_left_only = List.filter (fn cstr => case cstr of Affine (Rat _, Rat _, Schematic _, Rat _) => true | _ => false) G
    val affine_right_unifiers =
      List.concat (List.map (fn Affine (Rat r, Rat a, X as Schematic _, Rat b) =>
        if <>/ (a, rat_zero)
        then [(X, Rat (// (-/ (r, b), a)))]
        else []
         | _ => raise UnexpectedMatch_RunConsistency_50
        ) affine_cst_left_only)
    (* - Affine relation with integer fixpoint [X = a * X + b] if [X] has a solution in ℚ *)
    val affine_fixpoint_var = List.filter (fn cstr => case cstr of Affine (X1 as Schematic _, Rat _, X2 as Schematic _, Rat _) => X1 = X2 | _ => false) G
    val affine_fixpoint_unifiers =
      List.concat (List.map (fn Affine (X as Schematic _, Rat a, Schematic _, Rat b) =>
        if <>/ (-/ (rat_one, a), rat_zero)
        then [(X, Rat (// (b, -/ (rat_one, a))))]
        else []
         | _ => raise UnexpectedMatch_RunConsistency_51
        ) affine_fixpoint_var)
    (* - Timestamp [H ⇓_σ C] and [X^σ_H = _ * _ + _], where [X^σ_H] is a variable [C] is a constant *)
    val affine_var_1 = List.filter (fn cstr => case cstr of Affine (Schematic _, _, _, _) => true | _ => false) G
    val affine_timestamp_1_var_unifiers =
    List.concat (List.concat (List.map (fn
      Affine (Schematic (clk1, k1), _, v, _) =>
        (List.map (fn
        Timestamp (clk2, k2, Rat x) => (
          if clk1 = clk2 andalso k1 = k2
          then [(Schematic (clk1, k1), Rat x)]
          else []) | _ => raise UnexpectedMatch_RunConsistency_52) timestamp_cst_right) | _ => raise UnexpectedMatch_RunConsistency_53) affine_var_1))
    (* - Timestamp [H ⇓_σ C] and [_ = X^σ_H * _ + _], where [X^σ_H] is a variable [C] is a constant *)
    val affine_var_2 = List.filter (fn cstr => case cstr of Affine (_, Schematic _, _, _) => true | _ => false) G
    val affine_timestamp_2_var_unifiers =
    List.concat (List.concat (List.map (fn
      Affine (v, Schematic (clk1, k1), _, _) =>
        (List.map (fn
        Timestamp (clk2, k2, Rat i) => (
          if clk1 = clk2 andalso k1 = k2
          then [(Schematic (clk1, k1), Rat i)]
          else []) | _ => raise UnexpectedMatch_RunConsistency_54) timestamp_cst_right) | _ => raise UnexpectedMatch_RunConsistency_55) affine_var_2))
    (* - Timestamp [H ⇓_σ C] and [_ = _ * X^σ_H + _], where [X^σ_H] is a variable [C] is a constant *)
    val affine_var_3 = List.filter (fn cstr => case cstr of Affine (_, _, Schematic _, _) => true | _ => false) G
    val affine_timestamp_3_var_unifiers =
    List.concat (List.concat (List.map (fn
      Affine (v, _, Schematic (clk1, k1), _) =>
        (List.map (fn
        Timestamp (clk2, k2, Rat i) => (
          if clk1 = clk2 andalso k1 = k2
          then [(Schematic (clk1, k1), Rat i)]
          else []) | _ => raise UnexpectedMatch_RunConsistency_56) timestamp_cst_right) | _ => raise UnexpectedMatch_RunConsistency_57) affine_var_3))
    (* - Timestamp [H ⇓_σ C] and [_ = _ * _ + X^σ_H], where [X^σ_H] is a variable [C] is a constant *)
    val affine_var_4 = List.filter (fn cstr => case cstr of Affine (_, _, _, Schematic _) => true | _ => false) G
    val affine_timestamp_4_var_unifiers =
    List.concat (List.concat (List.map (fn
      Affine (v, _, _, Schematic (clk1, k1)) =>
        (List.map (fn
        Timestamp (clk2, k2, Rat i) => (
          if clk1 = clk2 andalso k1 = k2
          then [(Schematic (clk1, k1), Rat i)]
          else []) | _ => raise UnexpectedMatch_RunConsistency_58) timestamp_cst_right) | _ => raise UnexpectedMatch_RunConsistency_59) affine_var_4))
    (* - Timestamp [H ⇓_σ C] and [X^σ_H = _], where [X^σ_H] is a variable [C] is a constant *)
    val affine_var_refl_1 = List.filter (fn cstr => case cstr of AffineRefl (Schematic _, _) => true | _ => false) G
    val affine_timestamp_refl_1_var_unifiers =
    List.concat (List.concat (List.map (fn
      AffineRefl (Schematic (clk1, k1), _) =>
        (List.map (fn
        Timestamp (clk2, k2, Rat i) => (
          if clk1 = clk2 andalso k1 = k2
          then [(Schematic (clk1, k1), Rat i)]
          else []) | _ => raise UnexpectedMatch_RunConsistency_60) timestamp_cst_right) | _ => raise UnexpectedMatch_RunConsistency_61) affine_var_refl_1))
    (* - Timestamp [H ⇓_σ C] and [_ = X^σ_H], where [X^σ_H] is a variable [C] is a constant *)
    val affine_var_refl_2 = List.filter (fn cstr => case cstr of AffineRefl (_, Schematic _) => true | _ => false) G
    val affine_timestamp_refl_2_var_unifiers =
    List.concat (List.concat (List.map (fn
      AffineRefl (_, Schematic (clk1, k1)) =>
        (List.map (fn
        Timestamp (clk2, k2, Rat i) => (
          if clk1 = clk2 andalso k1 = k2
          then [(Schematic (clk1, k1), Rat i)]
          else []) | _ => raise UnexpectedMatch_RunConsistency_62) timestamp_cst_right) | _ => raise UnexpectedMatch_RunConsistency_63) affine_var_refl_2))
    (* Interpretable functions *)
    (* - Timestamps [H ⇓_σ1 C1], [H ⇓_σ2 C2]...
	  and [X^σ_H = f (X^σ1_H1, X^σ2_H2...)], where [X^σ_H] is a variable [C1], [C2]... are constants *)
    val funrel_var = List.filter (fn cstr => case cstr of FunRel (Schematic _, _, _) => true | _ => false) G
    fun are_fixed G schem_list =
	 List.all (fn Schematic (clk, k) =>
			List.exists (fn Timestamp (clk', k', Rat _) => clk = clk' andalso k = k'
				     | _ => false) G
		   | _ => raise UnexpectedMatch_RunConsistency_64)
		   schem_list
    val funrel_timestamp_unifiers = (* [(Schematic (clk, k), Rat ...)] *)
    List.concat (List.map (fn FunRel (Schematic (clk, k), Fun fname, schem_list) =>
			     if (is_interpretable G fname schem_list) andalso are_fixed G schem_list (* all schems are fixed *)
			     then [(Schematic (clk, k), Rat (interpret_fun G fname schem_list))]
			     else []
			     | _ => raise UnexpectedMatch_RunConsistency_65) funrel_var)
(*
    List.concat (List.concat (List.map (fn
      FunRel (Schematic (clk, k), fname, schem_list) =>
        (List.map (fn
        Timestamp (clk_cst, k_cst, Rat i) => (
          if clk1 = clk2 andalso k1 = k2 andalso (is_interpretable fname)
          then [(Schematic (clk, k), Rat i)]
          else [])) timestamp_cst_right)) funrel_var))
*)
  in
    timestamp_unifiers
    @ timestamp_add_schem_left_unifiers
    @ timestamp_add_schem_right_unifiers
    @ timestamp_add_csts_unifiers
    @ affine_left_unifiers
    @ affine_right_unifiers
    @ affine_fixpoint_unifiers
    @ affine_timestamp_1_var_unifiers
    @ affine_timestamp_2_var_unifiers
    @ affine_timestamp_3_var_unifiers
    @ affine_timestamp_4_var_unifiers
    @ affine_timestamp_refl_1_var_unifiers
    @ affine_timestamp_refl_2_var_unifiers
    @ funrel_timestamp_unifiers
  end;

(* Reconvert reflexive affines into affines *)
fun refl_affines_into_affine (G: system) =
    List.map (fn
		 AffineRefl (X, Rat r) => Affine (X, Rat rat_one, Rat r, Rat rat_zero)
	      | AffineRefl (Rat r, X) => Affine (X, Rat rat_one, Rat r, Rat rat_zero)
	      | AffineRefl (X, Int n) => Affine (X, Int 1, Int n, Int 0)
	      | AffineRefl (Int n, X) => Affine (X, Int 1, Int n, Int 0)
	      | g => g) G

(* Constant propagation main step *)
fun constants_propagation_step_int (G: system) =
  List.foldl (fn (subst, h) => apply_tag_substition subst h) G (constants_propagation_candidates_int G);
fun constants_propagation_step_rat (G: system) =
  List.foldl (fn (subst, h) => apply_tag_substition subst h) G (constants_propagation_candidates_rat G);
fun constants_propagation (G: system) =
  lfp (refl_affines_into_affine o constants_propagation_step_rat o constants_propagation_step_int) G

(* Remove trivial schematic timestamp of the kind [H ⇓_n X^H_n]*)
fun no_trivial_schem_timestamp (G: system) =
  List.filter (fn Timestamp (c, n, Schematic (c', n')) => not (c = c' andalso n = n') | _ => true) G

(* Affine equations with (only) constants elimination *)
fun constant_affine_eqns_elim (G: system) : system =
  List.filter (fn
      Affine (Int t1, Int a, Int t2, Int b) => t1 <> a * t2 + b
    | Affine (Rat t1, Rat a, Rat t2, Rat b) => <>/ (t1, +/ ( */ (a, t2), b))
    | _ => true) G

(* Elimination of equations with functions that are already interpreted *)
fun constant_funrel_eqns_elim (G: system) : system =
  List.filter
      (fn
	   FunRel (Schematic (clk, k), fname, tlist) =>
	    (not (List.exists (fn Timestamp (clk', k', Rat _) => clk = clk' andalso k = k' | _ => false) G
		  andalso (List.all (fn Schematic (clk, k) =>
					   List.exists (fn Timestamp (clk', k', _) => clk = clk' andalso k = k' | _ => false) G
				      | _ => true) tlist)))
      | _ => true) G
      
(* Equation [X = a * X + b] has a solution:
   - in ℤ iff [b] is dividable by [(1 - a)]
   - in ℚ iff [(1 - a)] ≠ 0
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
   - in ℚ iff [a] ≠ 0
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
fun decide (declared_quantities: clock list) (G: system) : bool =
           check_non_contradictory_ticks G
  andalso check_injectivity_on_timestamps G
  andalso check_injectivity_on_timestamps_varadd G
  andalso check_ascending_chain_on_timestamps declared_quantities G
  andalso check_type_consistency G
  andalso check_fixpoint_affeqns G
  andalso check_constants_affeqns G
  (* UNSAFE: Please investigate *)
  (* andalso check_no_shared_var_affeqns G *)
  andalso check_varright_affeqns G;

fun reduce (G: system) =
  no_trivial_schem_timestamp (constant_funrel_eqns_elim (constant_affine_eqns_elim (all_schematic_elim (constants_propagation (uniq G)))))

fun SAT (declared_quantities: clock list) (G: system) : bool =
  let val G_prop_and_elim_until_fp = lfp (reduce) G (* Keep reducing *)
  in decide declared_quantities G_prop_and_elim_until_fp                 (* Then decide! *)
  end

fun context_SAT (declared_quantities: clock list) ((G, _, _, _) : TESL_ARS_conf) =
  SAT declared_quantities G
