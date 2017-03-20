type rat = int * int
exception DenominatorIsZero
exception NegativeExponent

fun gcd (p: int, q: int) =
  if p = 0 then q else gcd (q mod p, p)

fun rat_normal ((p, q): rat): rat = (* q is assumed to be positive *)
  let
    val (p, q) = if q > 0 then (p, q) else (~p, ~q)
    val gcd0 = if p > 0 then gcd (p, q) else gcd (~p, q)
  in
    (p div gcd0, q div gcd0)
  end

fun rat_make (p: int, q: int): rat =
  if q = 0 then raise DenominatorIsZero
  else rat_normal (p, q)

fun op ~/ (p, q) =
  rat_normal (~p, q);

fun op +/ ((p, q), (p', q')) =
  rat_normal (p * q' + p' * q, q * q');

fun op -/ ((p, q), (p', q')) =
  rat_normal (p * q' - p' * q, q * q');

fun op */ ((p, q), (p', q')) =
  rat_normal (p * p', q * q');

fun op // ((p, q), (p', q')) =
  rat_normal (p * q', q * p');

fun op =/ (x, x') =
  (rat_normal x) = (rat_normal x')

fun op <>/ (x, x') =
  (rat_normal x) <> (rat_normal x')

val rat_zero = (0, 1);
val rat_one = (1, 1);

fun op </ ((p, q), (p', q')) =
  let
    val ((p, q), (p', q')) = ((p * q', q * q'), (p' * q, q * q'))
  in
    p < p'
  end

fun op <=/ ((p, q), (p', q')) =
  let
    val ((p, q), (p', q')) = ((p * q', q * q'), (p' * q, q * q'))
  in
    p <= p'
  end

fun string_of_rat ((p, q): rat) : string =
  let
      val as_real = (Real.fromInt p) / (Real.fromInt q)
      val {frac: real, whole: real} = Real.split as_real
  in
      if Real.== (frac, 0.0)
      then (Real.toString as_real) ^ "."
      else Real.toString as_real
  end

(* WARNING: Using Real.fromString is dangerous.
   Gets buggy for 0.111
*)
(*
fun rat_of_real (r: real) =
  let fun loop (r: real) (exp: int) =
    let val {frac: real, whole: real} = Real.split r in
      if Real.== (frac, 0.0)
      then (Real.floor r, exp)
      else loop (r * 10.0) (10 * exp)
    end
  in loop r 1
  end

fun rat_of_string (s: string) : rat option =
  case Real.fromString s of
      NONE   => NONE
    | SOME r => SOME (rat_of_real r)
*)
fun rat_of_int (n: int): rat =
  (n, 1)

fun string_length (s: string) =
  List.length (String.explode s)

fun digits_of_string (s: string) =
  let
      fun empty_to_zero s = case s of "" => "0" | _ => s
      val dotpos = ref 0
      val dotstate = ref false
      val (whole, frac) =
	   case List.find (fn #"." => true
			    | _ => (if !dotstate then () else dotpos := !dotpos + 1 ; false)) (String.explode s) of
		NONE   =>
		(empty_to_zero s, "0")
	     | SOME _ =>
		(empty_to_zero (String.substring (s, 0, !dotpos)),
		 empty_to_zero (String.substring (s, !dotpos + 1, (string_length s) - (!dotpos + 1))))
  in (whole, frac)
  end

fun exp a b =
  case b of
      0 => 1
    | 1 => a
    | _ => if b < 0 then raise NegativeExponent else a * (exp a (b - 1))

fun rat_of_digits (whole: string, frac: string): rat option =
  case (Int.fromString whole, Int.fromString frac) of
    (SOME w, SOME f) => SOME (+/ (rat_of_int w,
				      (f, exp 10 (List.length (String.explode frac)))))
  | _                => NONE

val rat_of_string =
  rat_of_digits o digits_of_string
