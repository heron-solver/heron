(**
   Module Hash

   Author : Hai Nguyen Van
            Université Paris-Saclay / CNRS
*)

signature HASH = sig
  val str_hash_of_TESL_conf: TESL_conf -> string

  (* Detect collisions*)
  val find_collision: TESL_conf list -> bool
  val is_forbidden: TESL_conf list -> int -> bool
end

structure Hash: HASH = struct
(* Addition and multiplication are provided with the Z/nZ group where n = MAXINT.
   Components for: - addition operate on Z/nZ where n = MAXINT / 2.
                   - multiplication operate on Z/nZ where n = √MAXINT.
*)
val plus = (fn (a, b) => a + b)
val mult = (fn (a, b) => a * b)
val MAXPLUS = MAXINT div 2
val MAXMULT = Real.floor (Math.sqrt (Real.fromInt (MAXINT)))
fun op + (a,b) = (plus (a mod MAXPLUS, b mod MAXPLUS)) mod MAXINT
fun op * (a,b) = (mult (a mod MAXMULT, b mod MAXMULT)) mod MAXINT

fun hash_of_string s =
    List.foldl (fn (c, res) => (Char.ord c) + res) 0 (String.explode s)

fun hash_of_clock_rel _ = 1 (* CURRENTLY: only one *)
fun hash_of_clock_expr _ = 0 (* POSSIBLY no need *)

fun hash_of_bool b = case b of
    true  => 2
  | false => 3
fun hash_of_clock (Clk (cname)) =
  hash_of_string cname
fun hash_of_clock_list cl =
  List.foldl (fn (c, res) => (hash_of_clock c)+res) 0 cl

fun hash_of_tag t = case t of
    Unit => 2
  | Int n => n
  | Rat (n1, n2) => Int.fromLarge (n1 mod (Int.toLarge MAXINT)) + Int.fromLarge (n2 mod (Int.toLarge MAXINT))
  | Schematic (c, n) => (hash_of_clock c) * n
  | Add (t1, t2) => (hash_of_tag t1) + (hash_of_tag t2)

fun hash_of_tag_list tl =
  List.foldl (fn (t, res) => (hash_of_tag t)+res) 0 tl
fun hash_of_tag_type tt = case tt of
    Unit_t => 2
  | Int_t  => 3
  | Rat_t  => 5
fun hash_of_fun (Fun (fname)) =
  hash_of_string fname

fun hash_of_primitive (prim: primitive): int =
  case prim of
    Timestamp (c, n, t)     => 2  * (hash_of_clock c) * n * (hash_of_tag t)
  | Ticks (c, n)            => 3  * (hash_of_clock c) * n
  | NotTicks (c, n)         => 5  * (hash_of_clock c) * n
  | NotTicksUntil (c, n)    => 7  * (hash_of_clock c) * n
  | NotTicksFrom (c, n)     => 11 * (hash_of_clock c) * n
  | Affine (t1, t2, t3, t4) => 13 * (hash_of_tag t1) * (hash_of_tag t2) * (hash_of_tag t3) * (hash_of_tag t4)
  | AffineRefl (t1, t2)     => 17 * (hash_of_tag t1) * (hash_of_tag t2)
  | FunRel (t, f, tl)       => 19 * (hash_of_tag t) * (hash_of_fun f) * (hash_of_tag_list tl)

fun hash_of_context (G: context): int =
  let fun aux (G: context) (res: int) =
    case G of
      []         => res
    | prim :: G' => aux G' (res + hash_of_primitive prim)
  in aux G 0
  end

fun hash_of_TESL_atomic (f: TESL_atomic): int = case f of
  True                                                  => 2
  | TypeDecl (c, tt, b)		                     => 3 	* (hash_of_clock c) * (hash_of_tag_type tt) * (hash_of_bool b)
  | Sporadic (c, t)			                     => 5  	* (hash_of_clock c) * (hash_of_tag t) 
  | Sporadics	(c, tl)		                     => 7  	* (hash_of_clock c) * (hash_of_tag_list tl) 
  | TypeDeclSporadics (tt, c, tl, b)                    => 11	* (hash_of_tag_type tt) * (hash_of_clock c) * (hash_of_tag_list tl) * (hash_of_bool b)
  | TagRelation (c_rel, c_e1, c_e2)                     => 13	* (hash_of_clock_rel c_rel) * (hash_of_clock_expr c_e1) * (hash_of_clock_expr c_e2)
  | TagRelationAff (c1, t1, c2, t2)	              => 17	* (hash_of_clock c1) * (hash_of_tag t1) * (hash_of_clock c2) * (hash_of_tag t2)
  | TagRelationCst (c, t)		                     => 19	* (hash_of_clock c) * (hash_of_tag t) 
  | TagRelationRefl (c1, c2)	                     => 23	* (hash_of_clock c1) * (hash_of_clock c2) 
  | TagRelationClk (c1, c2, c3, c4)                     => 29	* (hash_of_clock c1) * (hash_of_clock c2) * (hash_of_clock c3) * (hash_of_clock c4)
  | TagRelationPre (c1, c2)		                     => 31	* (hash_of_clock c1) * (hash_of_clock c2)
  | TagRelationFby (c1, tl, c2)	                     => 37	* (hash_of_clock c1) * (hash_of_tag_list tl) * (hash_of_clock c2)
  | TagRelationFun (c, f, cl)	                     => 41	* (hash_of_clock c) * (hash_of_fun f) * (hash_of_clock_list cl)
  | TagRelationDer (c1, c2)		                     => 43	* (hash_of_clock c1) * (hash_of_clock c2)
  | TagRelationReflImplies (c1, c2, c3)	              => 47	* (hash_of_clock c1) * (hash_of_clock c2) * (hash_of_clock c3)
(*| Implies (c1, c2)			                     => 53	* (hash_of_clock c1) * (hash_of_clock c2) *)
  | ImpliesGen (masters, slaves)	                     => 53	* (List.foldl (fn (x, res) => hash_of_clock x * res) 1 masters) * (List.foldl (fn (x, res) => hash_of_clock x * res) 1 slaves)
  | ImpliesNot (c1, c2)		                     => 59	* (hash_of_clock c1) * (hash_of_clock c2)
  | TimeDelayedBy (c1, t, c2, c3, c4)	              => 61	* (hash_of_clock c1)
								   * (hash_of_tag t)
								   * (hash_of_clock c2)
								   * (case c3 of NONE => 1 | SOME c3' => hash_of_clock c3')
								   * (hash_of_clock c4)
  | TimeDelayedBy_Abs (c1, c2, c3, c4)	              => 61	* (hash_of_clock c1) (* BETA *)
								   * (hash_of_clock c2)
								   * (case c3 of NONE => 1 | SOME c3' => hash_of_clock c3')
								   * (hash_of_clock c4)  | SporadicOn (c1, t, c2)	                     => 67	* (hash_of_clock c1) * (hash_of_tag t) * (hash_of_clock c2)
  | SporadicOn_Abs (c1, c2)	                     => 67	* (hash_of_clock c1) * (hash_of_clock c2) (* BETA *)
  | SporadicOnWithReset (c1, t, c2, c3) 		=> 71	* (hash_of_clock c1) * (hash_of_tag t) * (hash_of_clock c2) * (hash_of_clock c3)
  | DelayedBy (c1, n, c2, c3)				=> 73	* (hash_of_clock c1) * n * (hash_of_clock c2) * (hash_of_clock c3)
  | TimesImpliesOn (c1, n, c2)			       => 79	* (hash_of_clock c1) * n * (hash_of_clock c2) 
  | ImmediatelyDelayedBy (c1, n, c2, c3)		       => 83	* (hash_of_clock c1) * n * (hash_of_clock c2) * (hash_of_clock c3)
  | FilteredBy (c1, n1, n2, n3, n4, c2)			=> 89	* (hash_of_clock c1) * n1 * n2 * n3 * n4 * (hash_of_clock c2)
  | SustainedFrom (c1, c2, c3, c4)			       => 97	* (hash_of_clock c1) * (hash_of_clock c2) * (hash_of_clock c3) * (hash_of_clock c4)
  | UntilRestart (c1, c2, c3, c4)			       => 101	* (hash_of_clock c1) * (hash_of_clock c2) * (hash_of_clock c3) * (hash_of_clock c4)
  | SustainedFromImmediately (c1, c2, c3, c4)	       => 103	* (hash_of_clock c1) * (hash_of_clock c2) * (hash_of_clock c3) * (hash_of_clock c4)
  | UntilRestartImmediately (c1, c2, c3, c4)		=> 107	* (hash_of_clock c1) * (hash_of_clock c2) * (hash_of_clock c3) * (hash_of_clock c4)
  | SustainedFromWeakly (c1, c2, c3, c4)		       => 109	* (hash_of_clock c1) * (hash_of_clock c2) * (hash_of_clock c3) * (hash_of_clock c4)
  | UntilRestartWeakly (c1, c2, c3, c4)		       => 113	* (hash_of_clock c1) * (hash_of_clock c2) * (hash_of_clock c3) * (hash_of_clock c4)
  | SustainedFromImmediatelyWeakly (c1, c2, c3, c4)     => 127	* (hash_of_clock c1) * (hash_of_clock c2) * (hash_of_clock c3) * (hash_of_clock c4)
  | UntilRestartImmediatelyWeakly (c1, c2, c3, c4)	=> 131	* (hash_of_clock c1) * (hash_of_clock c2) * (hash_of_clock c3) * (hash_of_clock c4)
  | Await (cl1, cl2, cl3, c)				=> 137	* (hash_of_clock_list cl1) * (hash_of_clock_list cl2) * (hash_of_clock_list cl3) * (hash_of_clock c)
  | WhenClock	(c1, c2, c3)			              => 139	* (hash_of_clock c1) * (hash_of_clock c2) * (hash_of_clock c3)
  | WhenNotClock (c1, c2, c3)			       => 149	* (hash_of_clock c1) * (hash_of_clock c2) * (hash_of_clock c3)
  | EveryImplies (c1, n1, n2, c2)			       => 151	* (hash_of_clock c1) * n1 * n2 * (hash_of_clock c2)
  | NextTo (c1, c2, c3)				       => 157	* (hash_of_clock c1) * (hash_of_clock c2) * (hash_of_clock c3)
  | Periodic (c, t1, t2)				       => 163	* (hash_of_clock c) * (hash_of_tag t1) * (hash_of_tag t2)
  | TypeDeclPeriodic (tt, c, t1, t2)			=> 167	* (hash_of_tag_type tt) * (hash_of_clock c) * (hash_of_tag t1) * (hash_of_tag t2)
  | Precedes (c1, c2, b)			              => 173	* (hash_of_clock c1) * (hash_of_clock c2) * (hash_of_bool b)
  | Excludes (c1, c2)			              => 179	* (hash_of_clock c1) * (hash_of_clock c2) 
  | Kills (c1, c2)				              => 181	* (hash_of_clock c1) * (hash_of_clock c2)
  | DirMaxstep _		                            => 0 
  | DirMinstep _			                     => 0 
  | DirHeuristic _			                     => 0 
  | DirDumpres			                     => 0 
  | DirScenario _			                     => 0 
  | DirRunStep			                     => 0 
  | DirStutter			                     => 0 
  | DirRun _				                     => 0 
  | DirSelect _				              => 0 
  | DirDrivingClock _			              => 0 
  | DirEventConcretize _		                     => 0 
  | DirOutputVCD			                     => 0 
  | DirOutputCSV _			                     => 0 
  | DirOutputTEX _			                     => 0 
  | DirPrint _				              => 0 
  | DirExit				                     => 0 
  | DirHelp				                     => 0 
  | DirUniq				                     => 0 
  | DirCTLFormula	_		                     => 0 


fun hash_of_TESL_formula (F: TESL_formula): int =
  List.foldl (fn (f, res) => (hash_of_TESL_atomic f)+res) 0 F

fun hash_of_TESL_conf
  ((G, n, phi, psi): TESL_conf): int =
    2 * (hash_of_context G)
  + 3 * n
  + 5 * (hash_of_TESL_formula phi)
  + 7 * (hash_of_TESL_formula psi)

fun str_hash_of_TESL_conf cf =
  Int.fmt StringCvt.HEX (hash_of_TESL_conf cf)

fun find_collision cfs =
  let val hashed = List.map (hash_of_TESL_conf) cfs
  in not (List.length (uniq hashed) = List.length cfs)
  end

fun is_forbidden cfs h =
    List.length (List.filter (fn h' => h' = h)
				 (List.map (hash_of_TESL_conf) cfs))
    > 1
end
