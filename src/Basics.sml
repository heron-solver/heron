(* WARNING *)
(* COMMENT THE FOLLOWING LINE IF YOU WISH TO USE Isabelle/jEdit IDE *)
fun writeln s = print (s ^ "\n")
fun string_of_int n = Int.toString n
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
