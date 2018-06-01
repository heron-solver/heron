(**
   Module Basics

   Author : Hai Nguyen Van
            LRI, Université Paris-Sud/CNRS
   
   The copyright to this code is held by Laboratoire de Recherche en
   Informatique, Université Paris-Sud/CNRS. All rights reserved. This
   file is distributed under the MIT License.
*)

(* WARNING *)
(* COMMENT THE FOLLOWING LINE IF YOU WISH TO USE Isabelle/jEdit IDE *)
fun writeln s = print (s ^ "\n")
fun string_of_int n = Int.toString n

fun minus_instead_of_tild (str: string): string =
  String.implode (List.map (fn chr => if chr = #"~" then #"-" else chr) (String.explode str))
fun string_of_int_minus n =
  minus_instead_of_tild (Int.toString n)

(* **************************************************************** *)

(* Some colors *)
val BOLD_COLOR      = "\u001B[1m"
val UNDERLINE_COLOR = "\u001B[4m"
val RED_COLOR       = "\u001B[31m"
val GREEN_COLOR     = "\u001B[32m"
val YELLOW_COLOR    = "\u001B[33m"
val BLUE_COLOR      = "\u001B[34m"
val RESET_COLOR     = "\u001B[0m"

(* Ranges integers [1 : n] *)
fun range n = let fun aux n' l = if n' = 0 then l else aux (n' - 1) (n' :: l) in aux n [] end;

(* Dummy constants *)
val MAXINT = valOf (Int.maxInt)
val MININT = valOf (Int.minInt)

(* Update operators for referenced lists *)
infix 1 <>> <>>>
fun rl <>> x    = rl := (!rl) @ [x]
fun rl <>>> rl' = rl := (!rl) @ rl'

(* Returns the sublist of [l1] without occurences of elements of [l2] *)
infix 1 @-
fun l1 @- l2 = List.filter (fn e1 => List.all (fn e2 => e1 <> e2) l2) l1;
fun is_empty l = case l of [] => true | _ => false
fun contains x l = List.exists (fn x' => x = x') l

(* Returns a list of unique elements *)
fun uniq l =
  let
    fun aux (x :: l') acc =
          if List.exists (fn x' => x' = x) acc
          then aux l' acc
          else aux l' (x :: acc)
      | aux [] acc             = acc
  in List.rev (aux l [])
  end

exception Assert_failure
fun assert b =
  if b then b else raise Assert_failure

(* Returns the minimum of a list [l] of elements totally ordered by [leq] *)
fun min_list (leq: 'a * 'a -> bool) (l: 'a list) : 'a option = case l of
    []      => NONE
  | [x]     => SOME x
  | x :: l' => SOME (List.foldl (fn (e, current_min) => if leq (e, current_min)
								then e
								else current_min) x l')

(* Returns the maximum of a list [l] of elements totally ordered by [leq] *)
fun max_list (leq: 'a * 'a -> bool) (l: 'a list) : 'a option = case l of
    []      => NONE
  | [x]     => SOME x
  | x :: l' => SOME (List.foldl (fn (e, current_max) => if leq (e, current_max)
								then current_max
								else e) x l')

(* Given a seed [s], returns a random integer in [i, j] int interval *)
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

fun assoc l x = case (List.find (fn (k, v) => k = x) l) of
  SOME (_, V) => V

exception Empty

fun max (a, b) = if a > b then a else b

fun largest [] = raise Empty 
 | largest [x] = x
 | largest (x::xs) = max(x, largest xs)
 