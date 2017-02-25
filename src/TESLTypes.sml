(* WARNING *)
(* COMMENT THE FOLLOWING LINE IF YOU WISH TO USE Isabelle/jEdit IDE *)
fun writeln s = print (s ^ "\n")
fun string_of_int n = Int.toString n
(* **************************************************************** *)

(* To easily read the code, you can remove the following string but warnings will appear:
 | _ => raise UnexpectedMatch
*)
exception UnexpectedMatch
exception Assert_failure;
datatype clock = Clk of string;
type instant_index = int;

(* Returns the sublist of [l1] without occurences of elements of [l2] *)
fun op @- (l1, l2) = List.filter (fn e1 => List.all (fn e2 => e1 <> e2) l2) l1;
fun is_empty l = case l of [] => true | _ => false
fun contains x l = List.exists (fn x' => x = x') l

val compose = fn (f, g) => fn x => f(g x)

fun assert b =
  if b then b else raise Assert_failure

datatype tag =
    Int of int
  | Unit
  | Schematic of clock * instant_index
  | Add of tag * tag

datatype constr =
    Timestamp of clock * instant_index * tag
  | Ticks     of clock * instant_index
  | NotTicks  of clock * instant_index
  | Affine    of tag * tag * tag * tag

type system = constr list

datatype TESL_atomic =
  True
  | Sporadic                  of clock * tag
  | Sporadics                 of clock * (tag list)            (* Syntactic sugar *)
  | TagRelation               of clock * tag * clock * tag
  | Implies                   of clock * clock
  | TimeDelayedBy             of clock * tag * clock * clock
  | WhenTickingOn             of clock * tag * clock           (* Intermediate Form *)
  | DelayedBy                 of clock * int * clock * clock
  | TimesImpliesOn            of clock * int * clock           (* Intermediate Form *)
  | FilteredBy                of clock * int * int * int * int * clock
  | SustainedFrom             of clock * clock * clock * clock
  | UntilRestart              of clock * clock * clock * clock (* Intermediate Form *)
  | SustainedFromImmediately  of clock * clock * clock * clock
  | UntilRestartImmediately   of clock * clock * clock * clock (* Intermediate Form *)
  | Await                     of clock list * clock list * clock list * clock
  | WhenClock                 of clock * clock * clock
  | WhenNotClock              of clock * clock * clock
  | EveryImplies              of clock * int * int * clock (* Syntactic sugar *)
  | NextTo                    of clock * clock * clock     (* Syntactic sugar *)
  | Periodic                  of clock * tag * tag         (* Syntactic sugar *)
  | DirMaxstep                of int
  | DirMinstep                of int
  | DirHeuristic              of string
  | DirDumpres

type TESL_formula = TESL_atomic list

type TESL_ARS_conf = system * instant_index * TESL_formula * TESL_formula

fun ConstantlySubs f = List.filter (fn f' => case f' of
    Implies _      => true
  | TagRelation _  => true
  | WhenClock _    => true
  | WhenNotClock _ => true
  | _             => false) f
fun ConsumingSubs f = List.filter (fn f' => case f' of
(*  Sporadic _       => true *) (* Removed as handled seperately in SporadicSubs *)
    WhenTickingOn _  => true
  | TimesImpliesOn _ => true
  | _                => false) f

(* Returns sporadic formulae that are relevant to get instantaneously reduced *)
(* This tweak is necessary to avoid the state space explosion problem *)
fun SporadicNowSubs (f : TESL_formula) : TESL_formula =
  let
    val sporadics = List.filter (fn Sporadic _ => true | _ => false) f
    fun earliest_sporadics (spors : TESL_atomic list) : TESL_atomic list =
      let
      fun aux spors kept = case spors of
        [] => kept
      (* Keep the smallest, otherwise add if not exists *)
      | Sporadic (clk, Int i) :: spors' => (case List.find (fn Sporadic (clk', _) => clk = clk' | _ => raise UnexpectedMatch) kept of
          NONE => aux spors' (Sporadic (clk, Int i) :: kept)
        | SOME (Sporadic (clk', Int i')) =>
          if i < i'
          then aux spors' (Sporadic (clk, Int i) :: (@- (kept, [Sporadic (clk', Int i')])))
          else aux spors' kept
        | _ => raise UnexpectedMatch
        )
      | _ => raise UnexpectedMatch
    in aux spors [] end
  in earliest_sporadics sporadics end

fun ReproductiveSubs f = List.filter (fn f' => case f' of
    DelayedBy _     => true
  | TimeDelayedBy _ => true
  | _               => false) f
fun SelfModifyingSubs f = List.filter (fn f' => case f' of
    FilteredBy _     => true
  | SustainedFrom _  => true
  | SustainedFromImmediately _  => true
(*| TimesImpliesOn _ => true *)
  | UntilRestart _   => true
  | UntilRestartImmediately _   => true
  | Await _          => true
  | _                => false) f

exception UnsupportedTESLOperator;
(* Rephrase TESL formulae with syntactic sugar *)
fun unsugar (f : TESL_formula) =
  List.concat (List.map (fn
	      Sporadics (master, tags)            => (List.map (fn t => Sporadic (master, t)) tags)
	    | EveryImplies (master, n, x, slave)  => [FilteredBy (master, x, 1, n - 1, 1, slave)]
	    | NextTo (master, master_next, slave) => [SustainedFromImmediately (master, master_next, master, slave)]
	    | Periodic (clk, period, offset)      => [Sporadic (clk, offset),
							    TimeDelayedBy (clk, period, clk, clk)]
	    | DirMinstep _   => []
	    | DirMaxstep _   => []
	    | DirHeuristic _ => []
	    | DirDumpres     => []
	    | fatom => [fatom]
  ) f)

(* Asserts if two tags have the same type *)
fun op ::~ (tag, tag') = case (tag, tag') of
    (Int _, Int _) => true
  | (Unit, Unit)   => true
  | _              => raise Assert_failure

(* Asserts if a tag is less or equal another when they are constants *)
fun op ::<= (tag, tag') = case (tag, tag') of
    (Int i1, Int i2) => i1 <= i2
  | (Unit, Unit)     => true
  | _                => raise Assert_failure

(* Decides if two lists contain the same elements exactly *)
fun op @== (l1, l2) =
           List.all (fn e1 => List.exists (fn e2 => e1 = e2) l2) l1
  andalso List.all (fn e2 => List.exists (fn e1 => e1 = e2) l1) l2
fun op @-- (l1, l2) = List.filter (fn e1 => List.all (fn e2 => not (@== (e1, e2))) l2) l1;


(* Decides if two configurations are structurally equivalent *)
fun cfs_eq ((G1, s1, phi1, psi1) : TESL_ARS_conf) ((G2, s2, phi2, psi2) : TESL_ARS_conf) : bool =
           @== (G1, G2)
  andalso s1 = s2
  andalso @== (phi1, phi2)
  andalso @== (psi1, psi2)

(* Computes the least fixpoint of a functional [ff] starting at [x] *)
fun lfp (ff: ''a -> ''a) (x: ''a) : ''a =
  (* What's the difference between ['a] and [''a] ? *)
  let val x' = ff x in
  (if x = x' then x else lfp (ff) x') end

(* Returns a list of unique elements *)
fun uniq G =
  let
    fun aux (constr :: G') acc =
          if List.exists (fn constr' => constr' = constr) acc
          then aux G' acc
          else aux G' (constr :: acc)
      | aux [] acc             = acc
  in List.rev (aux G [])
  end

(* Removes redundants ARS configurations *)
fun cfl_uniq (cfl : TESL_ARS_conf list) : TESL_ARS_conf list =
  let
    fun aux (cf :: cfl') acc =
          if List.exists (fn cf' => cfs_eq cf cf') acc
          then aux cfl' acc
          else aux cfl' (cf :: acc)
      | aux [] acc             = acc
  in List.rev (aux cfl [])
  end

fun string_of_tag t = case t of
    Int n => string_of_int n
  | _     => "<tag>"
	      
fun string_of_clk c = case c of
    Clk cname => cname

fun string_of_expr e = case e of
    Sporadic (c, t)                                         => (string_of_clk c) ^ " sporadic " ^ (string_of_tag t)
  | Sporadics (c, tags)                                     => (string_of_clk c) ^ " sporadic " ^ (List.foldr (fn (t, s) => (string_of_tag t) ^ ", " ^ s) "" tags)
  | Implies (master, slave)                                 => (string_of_clk master) ^ " implies " ^ (string_of_clk slave)
  | TagRelation (c1, a, c2, b)                              => "tag relation " ^ (string_of_clk c1) ^ " = " ^ (string_of_tag a) ^ " * " ^ (string_of_clk c2) ^ " + " ^ (string_of_tag b)
  | TimeDelayedBy (master, t, measuring, slave)             => (string_of_clk master) ^ " time delayed by " ^ (string_of_tag t) ^ " on " ^ (string_of_clk measuring) ^ " implies " ^ (string_of_clk slave)
  | DelayedBy (master, n, counting, slave)                  => (string_of_clk master) ^ " delayed by " ^ (string_of_int n) ^ " on " ^ (string_of_clk counting) ^ " implies " ^ (string_of_clk slave)
  | FilteredBy (master, s, k, rs, rk, slave)                => (string_of_clk master) ^ " filtered by " ^ (string_of_int s) ^ ", " ^ (string_of_int k) ^ " (" ^ (string_of_int rs) ^ ", " ^ (string_of_int rk) ^ ")* implies " ^ (string_of_clk slave)
  | SustainedFrom (master, beginclk, endclk, slave)            => (string_of_clk master) ^ " sustained from " ^ (string_of_clk beginclk) ^ " to " ^ (string_of_clk endclk) ^ " implies " ^ (string_of_clk slave)
  | SustainedFromImmediately (master, beginclk, endclk, slave) => (string_of_clk master) ^ " immediately sustained from " ^ (string_of_clk beginclk) ^ " to " ^ (string_of_clk endclk) ^ " implies " ^ (string_of_clk slave)
  | Await (masters, _, _, slave)                            => "await " ^ (List.foldr (fn (clk, s) => (string_of_clk clk) ^ " " ^ s) "" masters) ^ "implies " ^ (string_of_clk slave)
  | WhenClock (m1, m2, slave)                               => (string_of_clk m1) ^ " when " ^ (string_of_clk m2) ^ " implies " ^ (string_of_clk slave)
  | WhenNotClock (m1, m2, slave)                            => (string_of_clk m1) ^ " when not " ^ (string_of_clk m2) ^ " implies " ^ (string_of_clk slave)
  | EveryImplies (master, n_every, n_start, slave)          => (string_of_clk master) ^ " every " ^ (string_of_int n_every) ^ " starting at " ^ (string_of_int n_start) ^  " implies " ^ (string_of_clk slave)
  | NextTo (c, next_c, slave)                               => (string_of_clk c) ^ " next to " ^ (string_of_clk next_c) ^ " implies " ^ (string_of_clk slave)
  | Periodic (c, per, offset)                               => (string_of_clk c) ^ " periodic " ^ (string_of_tag per) ^ " offset " ^ (string_of_tag offset)
  | _                                                       => "<tesl>"

