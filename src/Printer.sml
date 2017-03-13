 (* Some colors *)
val BOLD_COLOR      = "\u001B[1m"
val UNDERLINE_COLOR = "\u001B[4m"
val RED_COLOR       = "\u001B[31m"
val GREEN_COLOR     = "\u001B[32m"
val YELLOW_COLOR    = "\u001B[33m"
val BLUE_COLOR      = "\u001B[34m"
val RESET_COLOR     = "\u001B[0m"

fun superscript_of_char (c : char) =
  case c of
      #"a" => "\225\181\131"
    | #"b" => "\225\181\135"
    | #"c" => "\225\182\156"
    | #"d" => "\225\181\136"
    | #"e" => "\225\181\137"
    | #"f" => "\225\182\160"
    | #"g" => "\225\181\141"
    | #"h" => "\202\176"
    | #"i" => "\226\129\177"
    | #"j" => "\202\178"
    | #"k" => "\225\181\143"
    | #"l" => "\203\161"
    | #"m" => "\225\181\144"
    | #"n" => "\226\129\191"
    | #"o" => "\225\181\146"
    | #"p" => "\225\181\150"
    | #"q" => "\234\157\176" (* dummy *)
    | #"r" => "\202\179"
    | #"s" => "\203\162"
    | #"t" => "\225\181\151"
    | #"u" => "\225\181\152"
    | #"v" => "\225\181\155"
    | #"w" => "\202\183"
    | #"x" => "\203\163"
    | #"y" => "\202\184"
    | #"z" => "\225\182\187"
    | _    => "\203\128"
fun superscript_of_string (s : string) =
  String.concat (List.map (superscript_of_char) (String.explode s))
		    
fun subscript_of_int (n : int) =
  case n of
      0 => "\226\130\128"
    | 1 => "\226\130\129" 
    | 2 => "\226\130\130" 
    | 3 => "\226\130\131" 
    | 4 => "\226\130\132" 
    | 5 => "\226\130\133" 
    | 6 => "\226\130\134" 
    | 7 => "\226\130\135" 
    | 8 => "\226\130\136" 
    | 9 => "\226\130\137"
    | _ => ((subscript_of_int (n div 10)) ^ (subscript_of_int (n mod 10)))

(* Just for testing *)
fun string_of_int_exp (n : int) =
  if n > 1000 andalso n mod 1000 = 0
  then (string_of_int (n div 1000)) ^ "E3"
  else string_of_int n
		 
fun string_of_tag_ugly (t : tag) =
  case t of
      Int n => string_of_int n
    | Unit  => "()"
    | Schematic (Clk c_str, n) => "X\226\135\167" ^ (string_of_int n) ^ "\226\135\169" ^ c_str
    | Add (t1, t2) => (string_of_tag_ugly t1) ^ " + " ^ (string_of_tag_ugly t2)
fun string_of_tag_fancy (t : tag) =
  case t of
      Int n => string_of_int_exp n
    | Unit  => "()"
    | Schematic (Clk c_str, n) => "X" ^ subscript_of_int n ^ superscript_of_string c_str
    | Add (t1, t2) => (string_of_tag_fancy t1) ^ " + " ^ (string_of_tag_fancy t2)

(* You may change this parameter, depending on your CLI abilities *)
val string_of_tag = (string_of_tag_fancy)

fun string_of_timestamp_constr c =
  case c of
      Timestamp (Clk cname, n, tag) => "X" ^ subscript_of_int n ^ superscript_of_string cname ^ " = " ^ string_of_tag tag
    | _ => raise UnexpectedMatch
fun string_of_affine_constr c =
  case c of
      Affine (t1, ta, t2, tb) => (string_of_tag t1) ^ " = " ^ (string_of_tag ta) ^ " * " ^  (string_of_tag t2) ^ " + " ^ (string_of_tag tb)
    | _ => raise UnexpectedMatch

(* Print HAA-system *)
fun print_system (step_index: int) (clocks: clock list) (G : system) =
  let
    val G = lfp (reduce) G
(*  val clocks =
      uniq (List.concat (List.map (fn Ticks (c, _) => [c] | NotTicks (c, _) => [c] | Timestamp (c, _, _) => [c] | Affine _ => []) G))
    val nb_instants =
      List.foldl
        (fn (x, x0) => if x >= x0 then x else x0)
        0
        (List.concat (List.map (fn Ticks (_, n) => [n] | NotTicks (_, n) => [n] | Timestamp (_, n, _) => [n] | Affine _ => []) G))
*)
    fun constrs_of_clk_instindex c n =
      List.filter (fn Ticks (c', n') => c = c' andalso n = n' | NotTicks (c', n') => c = c' andalso n = n' | Timestamp (c', n', _) => c = c' andalso n = n' | _ => false) G
    fun string_of_constrs_at_clk_instindex clk n g =
      let
        val timestamps = List.filter (fn Timestamp (_, _, tag) => (case tag of Int _ => true | Unit => true | _ => false) | _ => false) g
      in
      if contains (Ticks (clk, n)) g andalso List.length timestamps > 0
      then "\226\135\145 " (* \<Up> *) ^ (string_of_tag (case List.nth (timestamps, 0) of Timestamp (_, _, tag) => tag | _ => raise UnexpectedMatch))
      else
        if contains (Ticks (clk, n)) g
        then "\226\135\145" (* \<Up> *)
        else
          if contains (NotTicks (clk, n)) g
          then "\226\138\152"  (* \<oslash> *)
          else
            if List.length timestamps > 0
            then "  " ^ (string_of_tag (case List.nth (timestamps, 0) of Timestamp (_, _, tag) => tag | _ => raise UnexpectedMatch))
            else ""
    end
    fun print_clocks () =
      writeln ("\t\t" ^ List.foldr (fn (Clk c, s) => c ^ "\t\t" ^ s) "" clocks)
    fun print_instant n =
      writeln ("[" ^ string_of_int n ^ "]"
		 ^ List.foldl (fn (c, s) => s ^ "\t\t" ^ string_of_constrs_at_clk_instindex c n (constrs_of_clk_instindex c n)) "" clocks)
    fun print_run k =
      if k > step_index
      then ()
      else (print_instant k ; print_run (k + 1))
  in print_clocks (); print_run 1
end

fun print_affine_constrs (G : system) : unit =
  let
      val affine_constrs =
	   List.filter (fn Affine _ => true | _ => false) G
      val nontriv_timestamps_constrs =
	   List.filter (fn Timestamp (_, _, Schematic _) => true | Timestamp (_, _, Add _) => true | _ => false) G
  in (case (affine_constrs, nontriv_timestamps_constrs) of ([], []) => () | _ => writeln "Affine constraints and non-trivial timestamps:" ;
      List.foldl (fn (c, _) => writeln ("\t" ^ (string_of_affine_constr c))) () affine_constrs ;
      List.foldl (fn (c, _) => writeln ("\t" ^ (string_of_timestamp_constr c))) () nontriv_timestamps_constrs)
  end
  
fun print_floating_ticks (clocks: clock list) (f: TESL_formula) : unit =
  let
    val sporadics = (List.filter (fn fatom => case fatom of Sporadic _ => true | _ => false) f)
    val whentickings = (List.filter (fn fatom => case fatom of WhenTickingOn _ => true | _ => false) f)
    fun string_of_sporadics c =
      List.foldl (fn (Sporadic (Clk clk, tag), s) =>
			if clk = c
			then (string_of_tag tag) ^ ", " ^ s
			else "" ^ s | _ => raise UnexpectedMatch) "" sporadics
    fun string_of_whentickingon c =
      List.foldl (fn (WhenTickingOn (Clk clk_meas_name, tag, Clk clk), s) =>
			if clk = c
			then "(" ^ BOLD_COLOR ^ "when" ^ RESET_COLOR ^ " " ^ (string_of_tag tag) ^ " " ^ BOLD_COLOR ^ "on" ^ RESET_COLOR ^ " " ^ clk_meas_name ^ "), " ^ s
			else "" ^ s | _ => raise UnexpectedMatch) "" whentickings
  in case (sporadics, whentickings) of
      ([], []) => ()
    | _ => (writeln "Floating ticks pending for merge:" ;
     List.app (fn (Clk cname) => writeln ("\t" ^ cname ^ ": " ^ (string_of_sporadics cname) ^ (string_of_whentickingon cname))) clocks)
  end
