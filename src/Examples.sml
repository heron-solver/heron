(* In the following examples, we recall the following constraint predicates of type [TESLTypes.constr]:
  - [Timestamp (c, \<sigma>, \<tau>)]  also seen as [c \<Down>\<^sub>\<sigma> \<tau>]         means clock [c] at instant [\<sigma>] has tag time [\<tau>]
  - [Ticks (c, \<sigma>)]         also seen as [c \<Up>\<^sub>\<sigma>]           means clock [c] at instant [\<sigma>] is hamletly ticking
  - [NotTicks (c, \<sigma>)]      also seen as [c \<not>\<Up>\<^sub>\<sigma>]          means clock [c] at instant [\<sigma>] is hamletly NOT ticking
  - [Affine (\<tau>, \<alpha>, \<tau>', \<beta>)] al so seen as [\<tau> = \<alpha> \<times> \<tau>' + \<beta>]  means to satisfy tag relation \<tau> = \<alpha> \<times> \<tau>' + \<beta>
*)

val default      = (~1, ~1, NONE)
val default_fast = (~1, ~1, SOME heuristic_minsporadic)

(* --- Example 1 ---
   Z-clock H1 sporadic 1, 2
   U-clock H2
   H1 implies H2
*)
val spec0 : TESL_formula = [
Sporadic (Clk "master", Int 1),
Sporadic (Clk "master", Int 2),
Implies (Clk "master", Clk "slave")];
val spec0_lim = solve spec0 (~1, ~1, SOME heuristic_minsporadic); 

(* --- Example 2 ---
   Z-clock master sporadic 1
   Z-clock meas
   U-clock slave
   tag relation master = meas
   master time delayed by 9 on meas implies slave
*)
val spec1 : TESL_formula = [
Sporadic (Clk "master", Int 1),
TagRelation (Clk "master", Int 1, Clk "meas", Int 0), 
TimeDelayedBy (Clk "master", Int 9, Clk "meas", Clk "slave")];
(* val spec1_lim = solve spec1 (3, ~1, NONE); *)

(* --- Example 3 ---
   Z-clock master sporadic 1
   master time delayed by 1 on master implies master
*)
val spec2 : TESL_formula = [Sporadic (Clk "master", Int 1), TimeDelayedBy (Clk "master", Int 1, Clk "master", Clk "master")];
(* WARNING: The following example is supposed to loop and will. Uncomment just for fun! *)
(* val spec2_lim = solve spec2 (~1, 5, SOME heuristic_minsporadic);  *)

(* --- Example 4 ---
   Z-clock master sporadic 1, 2, 3, 4, 5
   master filtered by 1, 2, (1, 2)* implies slave
*)
val spec3 : TESL_formula = [
  Sporadic (Clk "master", Int 1),
  Sporadic (Clk "master", Int 2),
  Sporadic (Clk "master", Int 3),
  Sporadic (Clk "master", Int 4),
  Sporadic (Clk "master", Int 5),
  FilteredBy (Clk "master", 1, 2, 1, 2, Clk "slave")];
(* val spec3_lim = solve spec3 default; *)

(* --- Example 4 ---
   Z-clock master sporadic 1
   Z-clock counting sporadic 2, 3
   tag relation master = counting
   master delayed by 2 on counting implies slave
*)
val spec4 : TESL_formula = [
  Sporadic (Clk "master", Int 1),
  Sporadic (Clk "countin", Int 2),
  Sporadic (Clk "countin", Int 3),
  TagRelation (Clk "master", Int 1, Clk "countin", Int 0),
  DelayedBy (Clk "master", 2, Clk "countin", Clk "slave")];
(* val spec4_lim = solve spec4 default; *)

val spec5 : TESL_formula = [
  Sporadic (Clk "master", Int 1),
  Sporadic (Clk "master", Int 2),
  Sporadic (Clk "master", Int 3),
  Sporadic (Clk "begin", Int 2),
  Sporadic (Clk "end", Int 3),
  TagRelation (Clk "master", Int 1, Clk "begin", Int 0),
  TagRelation (Clk "master", Int 1, Clk "end", Int 0),
  SustainedFrom (Clk "master", Clk "begin", Clk "end", Clk "slave")];
(* val spec5_lim = solve spec5 default; *)

val spec6 : TESL_formula = [
  Sporadic (Clk "m1", Int 1),
  Sporadic (Clk "m1", Int 3),
  Sporadic (Clk "m2", Int 2),
  Sporadic (Clk "m2", Int 3),
  TagRelation (Clk "m1", Int 1, Clk "m2", Int 0),
  Await ([Clk "m1", Clk "m2"], [Clk "m1", Clk "m2"], [Clk "m1", Clk "m2"], Clk "slave")];
(*  val spec6_lim = solve spec6 default; *)
(*  val spec6_lim = solve spec6 default_fast; *)
(* HEURISTIC IS BUGGY FOR THIS SPECIFICATION, SO IT HAS BEEN TWEAKED FOR *)

(* Example extracted from the website. Third one in http://wwwdi.supelec.fr/software/TESL/#FirstExample *)
val spec7 : TESL_formula = [
  Sporadic (Clk "m1", Int 1),
  Sporadic (Clk "m1", Int 4),
  Sporadic (Clk "m1", Int 7),
  Sporadic (Clk "m2", Int 2),
  Sporadic (Clk "m2", Int 5),
  TagRelation (Clk "m1", Int 1, Clk "m2", Int 0),
  Implies (Clk "m1", Clk "slave"),
  Implies (Clk "m1", Clk "slave")
];
(* val spec7_lim = solve spec7 (~1, ~1, SOME heuristic_minsporadic); *)
(* val spec7_lim = solve spec7 default; *)


(* Example of filtered-by with "hole" in master clock *)
val spec8 : TESL_formula = [
  Sporadic (Clk "master", Int 1),
  Sporadic (Clk "hole", Int 2),
  Sporadic (Clk "master", Int 3),
  Sporadic (Clk "master", Int 4),
  Sporadic (Clk "master", Int 5),
  Sporadic (Clk "master", Int 6),
  Sporadic (Clk "master", Int 7),
  TagRelation (Clk "master", Int 1, Clk "hole", Int 0),
  FilteredBy (Clk "master", 1, 2, 2, 2, Clk "slave")
];

(* val spec8_lim = solve spec8 (~1, ~1, SOME heuristic_minsporadic); *)
(* val spec8_lim = solve spec8 default; *)

(* Example with periodic clock *)
val spec9 : TESL_formula = [
  Sporadic (Clk "master", Int 1),
  TimeDelayedBy (Clk "master", Int 1, Clk "master", Clk "master"),
  FilteredBy (Clk "master", 3, 4, 1, 2, Clk "slave")
];
(* val spec9_lim = solve spec9 (~1, 3, SOME heuristic_minsporadic); *)
(* val spec9_lim = solve spec9 default; *)

(* Example of a when clock *)
val spec10 : TESL_formula = [
  Sporadic (Clk "master", Int 1),
  Sporadic (Clk "master", Int 2),
  Sporadic (Clk "master", Int 3),
  Sporadic (Clk "sampl", Int 2),
  TagRelation (Clk "master", Int 1, Clk "sampl", Int 0),
  WhenClock (Clk "master", Clk "sampl", Clk "slave")
];
(* val spec10_lim = solve spec10 (~1, 4, SOME heuristic_minsporadic); *)

(* Example of solvable specification here, but not in TESL solver*)
val spec11 : TESL_formula = [
  Sporadic (Clk "master", Int 1),
  Sporadic (Clk "master", Int 3),
  Sporadic (Clk "sampl", Int 2),
  TagRelation (Clk "master", Int 1, Clk "sampl", Int 0),
  WhenNotClock (Clk "master", Clk "sampl", Clk "sampl")
];
(* val spec11_lim = solve spec11 (7, ~1, SOME heuristic_minsporadic); *)


fun main () = ()
