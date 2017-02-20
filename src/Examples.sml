fun main () = ()

val default          = (~1, ~1, ([]:system), NONE)
val default_fast     = (~1, ~1, ([]:system), SOME heuristic_monotonic_sporadic)
val default_veryfast = (~1, ~1, ([]:system), SOME (compose (heuristic_monotonic_sporadic, heuristic_minsporadic)))

(* --- Example 1 ---
   Z-clock H1 sporadic 1, 2
   U-clock H2
   H1 implies H2
*)
val spec0 : TESL_formula = [
Sporadic (Clk "master", Int 1),
Sporadic (Clk "master", Int 2),
Implies (Clk "master", Clk "slave")];
(* val spec0_lim = solve spec0 (3, ~1, SOME heuristic_minsporadic); *)

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
(* val spec1_lim = solve spec1 (~1, ~1, [], SOME heuristic_minsporadic); *)

(* --- Example 3 ---
   Z-clock master sporadic 1
   master time delayed by 1 on master implies master
*)
val spec2 : TESL_formula = [
Sporadic (Clk "master", Int 1),
TimeDelayedBy (Clk "master", Int 2, Clk "master", Clk "master")];
(* WARNING: The following example is supposed to loop and will. Uncomment just for fun! *)
(* val spec2_lim = solve spec2 (~1, ~1, NONE); *)

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
(* val spec3_lim = solve spec3 default_fast; *)

(* --- Example 4 ---
   Z-clock master sporadic 1
   Z-clock counting sporadic 2, 3
   tag relation master = counting
   master delayed by 2 on counting implies slave
*)
val spec4 : TESL_formula = [
  Sporadic (Clk "master", Int 1),
  Sporadic (Clk "cntin", Int 2),
  Sporadic (Clk "cntin", Int 3),
  TagRelation (Clk "master", Int 1, Clk "cntin", Int 0),
  DelayedBy (Clk "master", 2, Clk "cntin", Clk "slave")];
(* val spec4_lim = solve spec4 default_fast; *)

val spec5 : TESL_formula = [
  Sporadic (Clk "master", Int 1),
  Sporadic (Clk "master", Int 2),
  Sporadic (Clk "master", Int 3),
  Sporadic (Clk "begin", Int 2),
  Sporadic (Clk "end", Int 3),
  TagRelation (Clk "master", Int 1, Clk "begin", Int 0),
  TagRelation (Clk "master", Int 1, Clk "end", Int 0),
  SustainedFrom (Clk "master", Clk "begin", Clk "end", Clk "slave")];
(* val spec5_lim = solve spec5 default_fast; *)

val spec6 : TESL_formula = [
  Sporadic (Clk "m1", Int 1),
  Sporadic (Clk "m1", Int 3),
  Sporadic (Clk "m2", Int 2),
  Sporadic (Clk "m2", Int 3),
  TagRelation (Clk "m1", Int 1, Clk "m2", Int 0),
  Await ([Clk "m1", Clk "m2"], [Clk "m1", Clk "m2"], [Clk "m1", Clk "m2"], Clk "slave")];
(* val spec6_lim = solve spec6 default_fast; *)
(*  val spec6_lim = solve spec6 default_veryfast; *)
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
  Implies (Clk "m2", Clk "slave")
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
(* val spec8_lim = solve spec8 default; *)

(* Example with periodic clock *)
val spec9 : TESL_formula = [
  Sporadic (Clk "master", Int 1),
  TimeDelayedBy (Clk "master", Int 1, Clk "master", Clk "master"),
  FilteredBy (Clk "master", 3, 4, 1, 2, Clk "slave")
];
(* WARNING: The following example is supposed to loop and will. Uncomment just for fun! *)
(* val spec9_lim = solve spec9 (~1, 3, SOME heuristic_minsporadic); *)
(* val spec9_lim = solve spec9 default_veryfast; *)

(* Example of a when clock *)
val spec10 : TESL_formula = [
  Sporadic (Clk "master", Int 1),
  Sporadic (Clk "master", Int 3),
  Sporadic (Clk "master", Int 4),
  Sporadic (Clk "master", Int 5),
  Sporadic (Clk "master", Int 7),
  Sporadic (Clk "master", Int 8),
  Sporadic (Clk "sampl", Int 2),
  Sporadic (Clk "sampl", Int 4),
  Sporadic (Clk "sampl", Int 6),
  Sporadic (Clk "sampl", Int 7),
  TagRelation (Clk "master", Int 1, Clk "sampl", Int 0),
  WhenClock (Clk "master", Clk "sampl", Clk "slave")
];
(* val spec10_lim = solve spec10 default_veryfast; *)
val codir10 : system = [
       Ticks(Clk "master", 1), NotTicks(Clk "sampl", 1), NotTicks(Clk "slave", 1),
    NotTicks(Clk "master", 2),    Ticks(Clk "sampl", 2), NotTicks(Clk "slave", 2),
       Ticks(Clk "master", 3), NotTicks(Clk "sampl", 3), NotTicks(Clk "slave", 3),
       Ticks(Clk "master", 4),    Ticks(Clk "sampl", 4),    Ticks(Clk "slave", 4),
       Ticks(Clk "master", 5), NotTicks(Clk "sampl", 5), NotTicks(Clk "slave", 5),
    NotTicks(Clk "master", 6),    Ticks(Clk "sampl", 6), NotTicks(Clk "slave", 6),
       Ticks(Clk "master", 7),    Ticks(Clk "sampl", 7),    Ticks(Clk "slave", 7),
       Ticks(Clk "master", 8), NotTicks(Clk "sampl", 8), NotTicks(Clk "slave", 8)
];
(* val spec10_lim = solve spec10 (~1, ~1, codir10, SOME (compose (heuristic_monotonic_sporadic, heuristic_minsporadic))); *)

(* Example of solvable specification here, but not in TESL solver*)
val spec11 : TESL_formula = [
  Sporadic (Clk "master", Int 1),
  Sporadic (Clk "master", Int 3),
  Sporadic (Clk "sampl", Int 2),
  TagRelation (Clk "master", Int 1, Clk "sampl", Int 0),
  WhenNotClock (Clk "master", Clk "sampl", Clk "sampl")
];
(* val spec11_lim = solve spec11 default; *)


val spec12 : TESL_formula = [
Sporadic (Clk "master1", Int 1),
Sporadic (Clk "master1", Int 2),
Sporadic (Clk "master2", Int 9),
Sporadic (Clk "master2", Int 3),
Implies (Clk "master2", Clk "slave")
];
(* val spec12_lim = solve spec12 (~1, ~1, [], SOME heuristic_monotonic_sporadic); *)

val spec13 : TESL_formula = [
    TagRelation (Clk "ms", Int 1000, Clk "sec", Int 0),
    TagRelation (Clk "sec", Int 60, Clk "min", Int 0),
    Sporadic (Clk "button", Int 60000),
    TagRelation (Clk "button", Int 1, Clk "ms", Int 0),
    Implies (Clk "button", Clk "switon"),
    TimeDelayedBy (Clk "switon", Int 10, Clk "min", Clk "switoff"),
    TimeDelayedBy (Clk "switon", Int 120, Clk "sec", Clk "lgton"),
    TimeDelayedBy (Clk "switoff", Int 60000, Clk "ms", Clk "lgtoff")
];

(* val spec13_lim = solve spec13 (~1, 2, NONE); *)

(* val spec13_lim = solve spec13 default; *)

(* val spec13_lim = solve spec13 (~1, 4, SOME heuristic_monotonic_sporadic);  *)
val codir13 : system = [
NotTicks(Clk "ms", 1), NotTicks(Clk "sec", 1), NotTicks(Clk "min", 1),    Ticks(Clk "button", 1),    Ticks(Clk "switon", 1), NotTicks(Clk "switoff", 1), NotTicks(Clk "lgton", 1), NotTicks(Clk "lgtoff", 1),
NotTicks(Clk "ms", 2), NotTicks(Clk "sec", 2), NotTicks(Clk "min", 2), NotTicks(Clk "button", 2), NotTicks(Clk "switon", 2), NotTicks(Clk "switoff", 2),    Ticks(Clk "lgton", 2), NotTicks(Clk "lgtoff", 2),
NotTicks(Clk "ms", 3), NotTicks(Clk "sec", 3), NotTicks(Clk "min", 3), NotTicks(Clk "button", 3), NotTicks(Clk "switon", 3),    Ticks(Clk "switoff", 3), NotTicks(Clk "lgton", 3), NotTicks(Clk "lgtoff", 3),
NotTicks(Clk "ms", 4), NotTicks(Clk "sec", 4), NotTicks(Clk "min", 4), NotTicks(Clk "button", 4), NotTicks(Clk "switon", 4), NotTicks(Clk "switoff", 4), NotTicks(Clk "lgton", 4),    Ticks(Clk "lgtoff", 4)
];
val spec13_lim = solve spec13 (~1, ~1, codir13, NONE);


val spec14 : TESL_formula = [
    SustainedFrom (Clk "allEvents", Clk "enter_Ready", Clk "leave_Ready", Clk "in_Ready"),
    SustainedFrom (Clk "allEvents", Clk "enter_Waiting", Clk "leave_Waiting", Clk "in_Waiting"),
    
    WhenClock (Clk "event_request", Clk "in_Ready", Clk "enter_Waiting"),
    WhenClock (Clk "event_request", Clk "in_Ready", Clk "leave_Ready"),
    WhenClock (Clk "event_request", Clk "in_Ready", Clk "action_reply"),

    WhenClock (Clk "event_ack", Clk "in_Waiting", Clk "enter_Ready"),
    WhenClock (Clk "event_ack", Clk "in_Waiting", Clk "leave_Waiting"),

    WhenClock (Clk "timeout_Waiting_to_Ready", Clk "in_Waiting", Clk "enter_Ready"),
    WhenClock (Clk "timeout_Waiting_to_Ready", Clk "in_Waiting", Clk "leave_Waiting"),
    WhenClock (Clk "timeout_Waiting_to_Ready", Clk "in_Waiting", Clk "action_reset"),

    Implies (Clk "event_ack", Clk "allEvents"),
    Implies (Clk "event_request", Clk "allEvents"),

    TimeDelayedBy (Clk "enter_Waiting", Int 10, Clk "chronometer", Clk "timeout_Waiting_to_Ready") (* Missing [with reset on] *),
    Implies (Clk "timeout_Waiting_to_Ready", Clk "allEvents"),

    Sporadic (Clk "start", Int 0),
    TagRelation (Clk "start", Int 1, Clk "chronometer", Int 0),
    Implies (Clk "start", Clk "allEvents"),
    Implies (Clk "start", Clk "enter_Ready"),

    TimeDelayedBy (Clk "start", Int 5, Clk "chronometer", Clk "event_request"),
    TimeDelayedBy (Clk "start", Int 13, Clk "chronometer", Clk "event_ack"),
    TimeDelayedBy (Clk "start", Int 19, Clk "chronometer", Clk "event_request")
];
(* val spec14_lim = solve spec14 default_fast;  *)
