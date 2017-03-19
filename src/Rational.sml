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
   Temporary solution.
   Gets buggy for 0.111
*)
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
