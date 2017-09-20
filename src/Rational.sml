(**
   Module Rational

   Author : Hai Nguyen Van
            LRI, Université Paris-Sud/CNRS
   
   The copyright to this code is held by Laboratoire de Recherche en
   Informatique, Université Paris-Sud/CNRS. All rights reserved. This
   file is distributed under the MIT License.
*)

type rat = LargeInt.int * LargeInt.int
exception DenominatorIsZero
exception NegativeExponent

fun gcd (p: LargeInt.int, q: LargeInt.int) =
  if p = (LargeInt.fromInt 0) then q else gcd (LargeInt.mod (q, p), p)

fun rat_normal ((p, q): rat): rat = (* q is assumed to be positive *)
  let
    val (p, q) = if q > 0 then (p, q) else (LargeInt.~p, LargeInt.~q)
    val gcd0 = if p > 0 then gcd (p, q) else gcd (LargeInt.~p, q)
  in
    (LargeInt.div (p, gcd0), LargeInt.div(q, gcd0))
  end

fun rat_make (p: int, q: int): rat =
  if (LargeInt.fromInt q) = (LargeInt.fromInt 0) then raise DenominatorIsZero
  else rat_normal (LargeInt.fromInt p, LargeInt.fromInt q)

fun op ~/ (p, q) =
  rat_normal (LargeInt.~p, q);

fun op +/ ((p, q), (p', q')) =
  (* rat_normal (p * q' + p' * q, q * q'); *)
  rat_normal (LargeInt.+ (LargeInt.* (p, q'), LargeInt.* (p', q)), LargeInt.* (q, q'));

fun op -/ ((p, q), (p', q')) =
  (* rat_normal (p * q' - p' * q, q * q'); *)
  rat_normal (LargeInt.- (LargeInt.* (p, q'), LargeInt.* (p', q)), LargeInt.* (q, q'));

fun op */ ((p, q), (p', q')) =
  (* rat_normal (p * p', q * q'); *)
  rat_normal (LargeInt.* (p, p'), LargeInt.* (q, q'));

fun op // ((p, q), (p', q')) =
  (* rat_normal (p * q', q * p'); *)
  rat_normal (LargeInt.* (p, q'), LargeInt.* (q, p'));

fun op =/ (x, x') =
  (rat_normal x) = (rat_normal x')

fun op <>/ (x, x') =
  (rat_normal x) <> (rat_normal x')

val rat_zero = (LargeInt.fromInt 0, LargeInt.fromInt 1);
val rat_one  = (LargeInt.fromInt 1, LargeInt.fromInt 1);

fun op </ ((p, q), (p', q')) =
  let
    (* val ((p, q), (p', q')) = ((p * q', q * q'), (p' * q, q * q')) *)
    val ((p, q), (p', q')) = ((LargeInt.* (p, q'), LargeInt.* (q, q')), (LargeInt.* (p', q), LargeInt.* (q, q')))
  in
    LargeInt.< (p, p')
  end

fun op <=/ ((p, q), (p', q')) =
  let
    val ((p, q), (p', q')) = ((LargeInt.* (p, q'), LargeInt.* (q, q')), (LargeInt.* (p', q), LargeInt.* (q, q')))
  in
    LargeInt.<= (p, p')
  end

fun string_of_rat ((p, q): rat) : string =
  let
      val as_real = (Real.fromLargeInt p) / (Real.fromLargeInt q)
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
fun rat_of_LargeInt (n: LargeInt.int): rat =
  (n, LargeInt.fromInt 1)

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
  in (case whole of "-" => "-0" | _ => whole, frac)
  end

fun exp (a: LargeInt.int) (b: LargeInt.int) =
  case b of
      0 => 1
    | 1 => a
    | _ => if b < 0 then raise NegativeExponent else a * (exp a (b - 1))

fun rat_of_digits (whole: string, frac: string): rat option =
  case (LargeInt.fromString whole, LargeInt.fromString frac) of
    (SOME w, SOME f) =>
    if w = 0
    then
	 case String.explode whole of
	     #"-" :: _ => SOME (-/ (rat_of_LargeInt w,
					(f, exp (LargeInt.fromInt 10) (LargeInt.fromInt (List.length (String.explode frac))))))
	   | _         => SOME (+/ (rat_of_LargeInt w,
					(f, exp (LargeInt.fromInt 10) (LargeInt.fromInt (List.length (String.explode frac))))))
    else
	 if w < 0
	 then SOME (-/ (rat_of_LargeInt w,
			  (f, exp (LargeInt.fromInt 10) (LargeInt.fromInt (List.length (String.explode frac))))))
	 else SOME (+/ (rat_of_LargeInt w,
			  (f, exp (LargeInt.fromInt 10) (LargeInt.fromInt (List.length (String.explode frac))))))
   | _                => NONE

val rat_of_string =
  rat_of_digits o digits_of_string

(* Given a seed [s], returns a random rational in [i, j] rat interval *)
(*
fun random_int_range (i: int, j: int) (s: word): int = 
  if j < i
  then raise Assert_failure
  else
    if j = i then i
    else
      let 
	 val m : Int32.int = 2147483647  (* 2^31 - 1 *)
	 val R = (Int32.fromInt j) - (Int32.fromInt i)
	 val cvt = Word.toIntX o Word.fromLargeInt o Int32.toLarge
      in
	 if R = m
	 then Word.toIntX s
	 else i + cvt (((Int32.fromLarge o Word.toLargeInt) s) mod (R+1))
      end
handle Overflow => random_int_range (i, j) (valOf (MLton.Random.useed ()));
*)

(* Random rational number between (0.0,1.0) *)
fun random_rat seed =
   let 
     val m : Int32.int = 2147483647  (* 2^31 - 1 *)
   in rat_normal (Word.toLargeInt seed, Int32.toLarge m)
   end
