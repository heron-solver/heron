(**
   Module TESL

   Author : Hai Nguyen Van
            LRI, Université Paris-Sud/CNRS
   
   The copyright to this code is held by Laboratoire de Recherche en
   Informatique, Université Paris-Sud/CNRS. All rights reserved. This
   file is distributed under the MIT License.
*)

(* To easily read the code, you can remove the following string but
   warnings will appear:
   | _ => raise UnexpectedMatch
*)
exception UnexpectedMatch
datatype clock = Clk of string
type instant_index = int
datatype func = Fun of string

exception UnsupportedParsedTerm

datatype tag =
    Unit
  | Int of int
  | Rat of rat
  | Schematic of clock * instant_index
  | Add of tag * tag

datatype constr =
    Timestamp of clock * instant_index * tag
  | Ticks     of clock * instant_index
  | NotTicks  of clock * instant_index
  | NotTicksUntil of clock * instant_index
  | NotTicksFrom  of clock * instant_index
  | Affine    of tag * tag * tag * tag        (* X1 = X2 * X3 + X4 *)
  | AffineRefl of tag * tag                   (* X = Y *)
  | FunRel of tag * func * tag list           (* X = f (X1, X2...) *)

type system = constr list

fun clocks_of_system (G: system) =
  uniq (List.concat (List.map (fn
    Timestamp (c, _, _)  => [c]
  | Ticks (c, _)         => [c]
  | NotTicks (c, _)      => [c]
  | NotTicksUntil (c, _) => [c]
  | NotTicksFrom (c, _)  => [c]
  | _ => []
  ) G))

fun is_tag_constant (t: tag) = case t of
    Unit  => true
  | Int _ => true
  | Rat _ => true
  | _ => false

datatype tag_t =
    Unit_t
  | Int_t
  | Rat_t

fun string_of_tag_t ty = case ty of
    Unit_t => "[unit]"
  | Int_t  => "[int]"
  | Rat_t  => "[rational]"

exception ImpossibleTagTypeInference
fun type_of_tag (t: tag) = case t of
    Unit  => Unit_t
  | Int _ => Int_t
  | Rat _ => Rat_t
  | _     => raise ImpossibleTagTypeInference

exception TagTypeInconsistency of clock * tag_t * tag_t
fun type_of_tags (clk: clock) (tlist: tag list) =
  case tlist of
      []          => Unit_t (* Questionable design choice *)
    | [t]         => type_of_tag t
    | t :: tlist' => if (type_of_tag t) = type_of_tags clk tlist'
			then type_of_tag t
			else raise TagTypeInconsistency (clk, type_of_tag t, type_of_tags clk tlist')

datatype TESL_atomic =
  True
  | TypeDecl                       of clock * tag_t * bool
  | Sporadic                       of clock * tag
  | Sporadics                      of clock * (tag list)            (* Syntactic sugar *)
  | TypeDeclSporadics              of tag_t * clock * (tag list) * bool   (* Syntactic sugar *)
  | TagRelation                    of clock * tag * clock * tag
  | TagRelationCst                 of clock * tag
  | TagRelationRefl                of clock * clock
  | TagRelationClk                 of clock * clock * clock * clock
  | TagRelationPre                 of clock * clock
  | TagRelationFby                 of clock * tag list * clock
  | TagRelationFun                 of clock * func * clock list
  | TagRelationDer                 of clock * clock
  | Implies                        of clock * clock
  | ImpliesNot                     of clock * clock
  | TimeDelayedBy                  of clock * tag * clock * (clock option) * clock
  | WhenTickingOn                  of clock * tag * clock
  | WhenTickingOnWithReset         of clock * tag * clock * clock   (* Intermediate Form *)
  | DelayedBy                      of clock * int * clock * clock
  | TimesImpliesOn                 of clock * int * clock           (* Intermediate Form *)
  | ImmediatelyDelayedBy           of clock * int * clock * clock
  | FilteredBy                     of clock * int * int * int * int * clock
  | SustainedFrom                  of clock * clock * clock * clock
  | UntilRestart                   of clock * clock * clock * clock (* Intermediate Form *)
  | SustainedFromImmediately       of clock * clock * clock * clock
  | UntilRestartImmediately        of clock * clock * clock * clock (* Intermediate Form *)
  | SustainedFromWeakly            of clock * clock * clock * clock
  | UntilRestartWeakly             of clock * clock * clock * clock (* Intermediate Form *)
  | SustainedFromImmediatelyWeakly of clock * clock * clock * clock
  | UntilRestartImmediatelyWeakly  of clock * clock * clock * clock (* Intermediate Form *)
  | Await                          of clock list * clock list * clock list * clock
  | WhenClock                      of clock * clock * clock
  | WhenNotClock                   of clock * clock * clock
  | EveryImplies                   of clock * int * int * clock (* Syntactic sugar *)
  | NextTo                         of clock * clock * clock     (* Syntactic sugar *)
  | Periodic                       of clock * tag * tag         (* Syntactic sugar *)
  | TypeDeclPeriodic               of tag_t * clock * tag * tag (* Syntactic sugar *)
  | Precedes                       of clock * clock * bool (* weakly*)
  | Excludes                       of clock * clock
  | Kills                          of clock * clock
  | DirMaxstep                     of int
  | DirMinstep                     of int
  | DirHeuristic                   of string
  | DirDumpres
  | DirScenario                    of bool * int option * (clock * tag option) list
                                      (* strict?, next or index, clk with tag (or not) *)
  | DirRunStep
  | DirRun
  | DirSelect                      of int
  | DirDrivingClock                of clock list
  | DirEventConcretize             of int option
  | DirOutputVCD
  | DirOutputTEX                   of bool * bool * (clock list)
  | DirPrint                       of clock list (* If empty then all clocks *)
  | DirExit
  | DirHelp

type TESL_formula = TESL_atomic list

type TESL_ARS_conf = system * instant_index * TESL_formula * TESL_formula

(* *** WARNING ***
 * TESL formulae must be clearly stated in one the following categories:
 * ConstantlySubs, ConsumingSubs, SporadicNowSubs, ReproductiveSubs, or SelfModifyingSubs
 * If not, they will be ignored at instant initialization...
 *)
fun ConstantlySubs f = List.filter (fn f' => case f' of
    Implies _        => true
  | ImpliesNot _     => true
  | TagRelation _    => true
  | TagRelationRefl _    => true
  | TagRelationCst _ => true
  | TagRelationClk _ => true
  | TagRelationPre _ => true
  | TagRelationFun _ => true
  | TagRelationDer _ => true
  | WhenClock _      => true
  | WhenNotClock _   => true
  | Precedes _       => true
  | Excludes _       => true
  | Kills _       => true
  | _             => false) f
fun ConsumingSubs f = List.filter (fn f' => case f' of
(*  Sporadic _       => true *) (* Removed as handled seperately in SporadicSubs *)
    WhenTickingOn _  => true
  | TimesImpliesOn _ => true
  | _                => false) f

fun SporadicQuantitiesSubs (declared_clocks: clock list) (f: TESL_formula) : TESL_formula =
    List.filter (fn
		    Sporadic (clk, t) => List.exists (fn clk' => clk = clk') declared_clocks
		  | _ => false) f

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
          then aux spors' (Sporadic (clk, Int i) :: (kept @- [Sporadic (clk', Int i')]))
          else aux spors' kept
        | _ => raise UnexpectedMatch
        )
      | Sporadic (clk, Rat x) :: spors' => (case List.find (fn Sporadic (clk', _) => clk = clk' | _ => raise UnexpectedMatch) kept of
          NONE => aux spors' (Sporadic (clk, Rat x) :: kept)
        | SOME (Sporadic (clk', Rat x')) =>
          if </ (x, x')
          then aux spors' (Sporadic (clk, Rat x) :: (kept @- [Sporadic (clk', Rat x')]))
          else aux spors' kept
        | _ => raise UnexpectedMatch
        )
      | Sporadic (clk, Unit) :: spors' => (case List.find (fn Sporadic (clk', _) => clk = clk' | _ => raise UnexpectedMatch) kept of
          NONE => aux spors' (Sporadic (clk, Unit) :: kept)
        | SOME _ => aux spors' kept
      )
      | _ => raise UnexpectedMatch
    in aux spors [] end
  in earliest_sporadics sporadics end

fun ReproductiveSubs f = List.filter (fn f' => case f' of
    DelayedBy _            => true
  | ImmediatelyDelayedBy _ => true
  | TimeDelayedBy _        => true
  | _                      => false) f
fun SelfModifyingSubs f = List.filter (fn f' => case f' of
    FilteredBy _                      => true
  | SustainedFrom _                   => true
  | SustainedFromWeakly _             => true
  | SustainedFromImmediately _        => true
  | SustainedFromImmediatelyWeakly _  => true
(*| TimesImpliesOn _ => true *)
  | UntilRestart _                    => true
  | UntilRestartWeakly _              => true
  | UntilRestartImmediately _         => true
  | UntilRestartImmediatelyWeakly _   => true
  | Await _                           => true
  | TagRelationFby _                  => true
  | _                                 => false) f

exception UnsupportedTESLOperator
exception UnitTagRelationFault

(* Eliminate TESL syntactic sugars *)
fun unsugar (clock_types: (clock * tag_t) list) (f : TESL_formula) =
  List.concat (List.map (fn
	      Sporadics (master, tags)             => (List.map (fn t => Sporadic (master, t)) tags)
	    (* TODO: How to type TagRelationRefl, TagRelationClk and TagRelationPre ? *)
           | TypeDeclSporadics (ty, master, tags, _) => unsugar clock_types [Sporadics (master, tags)]
           | TypeDecl (ty, clk, _)                => []
	    | EveryImplies (master, n, x, slave)   => [FilteredBy (master, x, 1, n - 1, 1, slave)]
	    | NextTo (master, master_next, slave)  => [SustainedFromImmediately (master, master_next, master, slave)]
	    | Periodic (clk, period, offset)       => [Sporadic (clk, offset),
							    TimeDelayedBy (clk, period, clk, NONE, clk)]
	    | TypeDeclPeriodic (ty, clk, period, offset) => unsugar clock_types [Periodic (clk, period, offset)]
           | TagRelationDer (c1, Clk c2_name) => [TagRelationCst (Clk "one", Rat rat_one),
							 TagRelationPre (Clk ("_pre_" ^ c2_name), Clk c2_name),
							 TagRelation (Clk ("_mpre_" ^ c2_name), Rat (~/ rat_one), Clk ("_pre_" ^ c2_name), Rat rat_zero),
							 TagRelationClk (c1, Clk "one", Clk c2_name, Clk ("_mpre_" ^ c2_name))]
	    | DirMinstep _          => []
	    | DirMaxstep _          => []
	    | DirHeuristic _        => []
	    | DirDumpres            => []
	    | DirScenario _         => []
	    | DirRun                => []
	    | DirRunStep            => []
	    | DirDrivingClock _     => []
	    | DirEventConcretize _  => []
	    | DirSelect _           => []
	    | DirPrint _            => []
	    | DirExit               => []
	    | DirHelp               => []
	    | DirOutputVCD          => []
	    | DirOutputTEX _        => []
	    | fatom => [fatom]
  ) f)

(* Asserts if two tags have the same type *)
fun op ::~ (tag, tag') = case (tag, tag') of
    (Unit, Unit)   => true
  | (Int _, Int _) => true
  | (Rat _, Rat _) => true
  | _              => raise Assert_failure

(* Asserts if a tag is less or equal another when they are constants *)
fun op ::<= (tag, tag') = case (tag, tag') of
    (Unit, Unit)     => true
  | (Int i1, Int i2) => i1 <= i2
  | (Rat x1, Rat x2) => <=/ (x1, x2)
  | _                => raise Assert_failure

(* Decides if two lists are set-interpreted equivalent *)
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
  let val x' = ff x in
  (if x = x' then x else lfp (ff) x') end

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
    Unit  => "()"
  | Int n => string_of_int n
  | Rat x => string_of_rat x
  | _     => "<tag>"

fun string_of_clk c = case c of
  Clk cname => cname

fun string_of_clks clist =
    String.concatWith " " (List.map (string_of_clk) clist)

fun string_of_tag_type ty = case ty of
    Unit_t => "unit"
  | Int_t =>  "int"
  | Rat_t =>  "rational"

fun clocks_of_tesl_formula (f : TESL_formula) : clock list =
  uniq (List.concat (List.map (fn
    TypeDecl (c, _, _)                     => [c]
  | Sporadic (c, _)                           => [c]
  | Sporadics (c, _)                          => [c]
  | WhenTickingOn (c1, _, c2)                 => [c1, c2]
  | TypeDeclSporadics (_, c, _, _)            => [c]
  | TagRelation (c1, _, c2, _)                => [c1, c2]
  | TagRelationCst (c, _)                     => [c]
  | TagRelationRefl (c1, c2)                  => [c1, c2]
  | TagRelationClk (c1, ca, c2, cb)           => [c1, ca, c2, cb]
  | TagRelationPre (c1, c2)                   => [c1, c2]
  | TagRelationFby (c1, _, c2)                => [c1, c2]
  | TagRelationFun (c, _, clist)              => [c] @ clist
  | TagRelationDer (c1, c2)                   => [c1, c2]
  | Implies (c1, c2)                          => [c1, c2]
  | ImpliesNot (c1, c2)                       => [c1, c2]
  | TimeDelayedBy (c1, _, c2, NONE, c3)       => [c1, c2, c3]
  | TimeDelayedBy (c1, _, c2, SOME (rc), c3)  => [c1, c2, rc, c3]
  | DelayedBy (c1, _, c2, c3)                 => [c1, c2, c3]
  | ImmediatelyDelayedBy (c1, _, c2, c3)      => [c1, c2, c3]
  | FilteredBy (c1, _, _, _, _, c2)           => [c1, c2]
  | SustainedFrom (c1, c2, c3, c4)            => [c1, c2, c3, c4]
  | SustainedFromImmediately (c1, c2, c3, c4) => [c1, c2, c3, c4]
  | SustainedFromWeakly (c1, c2, c3, c4)            => [c1, c2, c3, c4]
  | SustainedFromImmediatelyWeakly (c1, c2, c3, c4) => [c1, c2, c3, c4]
  | Await (clks, _, _, c)                     => clks @ [c]
  | WhenClock (c1, c2, c3)                    => [c1, c2, c3]
  | WhenNotClock (c1, c2, c3)                 => [c1, c2, c3]
  | EveryImplies (c1, _, _, c2)               => [c1, c2]
  | NextTo (c1, c2, c3)                       => [c1, c2, c3]
  | Periodic (c, _, _)                        => [c]
  | TypeDeclPeriodic (_, c, _, _)             => [c]
  | Precedes (c1, c2, _)                      => [c1, c2]
  | Excludes (c1, c2)                         => [c1, c2]
  | Kills (c1, c2)                            => [c1, c2]
  | DirScenario (_, _, tclks)                 => List.map (fn (clk, _) => clk) tclks
  | DirDrivingClock clks                      => clks
  | _ => []
  ) f))  

fun quantities_of_tesl_formula (f : TESL_formula) : clock list =
  uniq (List.concat (List.map (fn
    TypeDecl (c, _, mono)                        => if not mono then [c] else []
  | TypeDeclSporadics (_, c, _, mono)            => if not mono then [c] else []
  | _ => []
  ) f))  

fun has_no_floating_ticks (f : TESL_formula) =
  (* Stop condition 1. No pending sporadics *)
  (List.length (List.filter (fn fatom => case fatom of Sporadic _ => true | _ => false) f) = 0)
  (* Stop condition 2. No pending whenticking *)
  andalso (List.length (List.filter (fn fatom => case fatom of WhenTickingOn _ => true | _ => false) f) = 0)
