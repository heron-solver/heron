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
