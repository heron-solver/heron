exception Assert_failure;
datatype clock = Clk of string;
type instant_index = int;

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
  | Sporadic       of clock * tag
  | TagRelation    of clock * tag * clock * tag
  | Implies        of clock * clock
  | TimeDelayedBy  of clock * tag * clock * clock
  | WhenTickingOn  of clock * tag * clock           (* Intermediate Form *)
  | DelayedBy      of clock * int * clock * clock
  | TimesImpliesOn of clock * int * clock           (* Intermediate Form *)
  | FilteredBy     of clock * int * int * int * int * clock
  | SustainedFrom  of clock * clock * clock * clock
  | UntilRestart   of clock * clock * clock * clock (* Intermediate Form *)
  | Await          of clock list * clock list * clock list * clock
  | WhenClock      of clock * clock * clock
  | WhenNotClock   of clock * clock * clock

type TESL_formula = TESL_atomic list

type TESL_ARS_conf = system * instant_index * TESL_formula * TESL_formula

fun ConstantlySubs f = List.filter (fn f' => case f' of
    Implies _      => true
  | TagRelation _  => true
  | WhenClock _    => true
  | WhenNotClock _ => true
  | _             => false) f
fun ConsumingSubs f = List.filter (fn f' => case f' of
    Sporadic _       => true
  | WhenTickingOn _  => true
  | TimesImpliesOn _ => true
  | _                => false) f
fun ReproductiveSubs f = List.filter (fn f' => case f' of
    DelayedBy _     => true
  | TimeDelayedBy _ => true
  | _               => false) f
fun SelfModifyingSubs f = List.filter (fn f' => case f' of
    FilteredBy _     => true
  | SustainedFrom _  => true
(*| TimesImpliesOn _ => true *)
  | UntilRestart _   => true
  | Await _          => true
  | _                => false) f

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


(* Decides if two configurations are structurally equivalent *)
fun cfs_eq ((G1, s1, phi1, psi1) : TESL_ARS_conf) ((G2, s2, phi2, psi2) : TESL_ARS_conf) : bool =
           @== (G1, G2)
  andalso s1 = s2
  andalso @== (phi1, phi2)
  andalso @== (psi1, psi2)

(* Returns the sublist of [l1] without occurences of elements of [l2] *)
fun op @- (l1, l2) = List.filter (fn e1 => List.all (fn e2 => e1 <> e2) l2) l1;
fun op @-- (l1, l2) = List.filter (fn e1 => List.all (fn e2 => not (@== (e1, e2))) l2) l1;
fun is_empty l = case l of [] => true | _ => false

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

fun writeln s = print (s ^ "\n")
fun string_of_int n = Int.toString n
