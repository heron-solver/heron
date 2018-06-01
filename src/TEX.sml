(**
   Module TEX

   Author : Hai Nguyen Van
            LRI, Université Paris-Sud/CNRS
   
   The copyright to this code is held by Laboratoire de Recherche en
   Informatique, Université Paris-Sud/CNRS. All rights reserved. This
   file is distributed under the MIT License.
*)

val string_of_int = string_of_int_minus

fun string_no_subscript str =
  let fun replace chrl = case chrl of
      [] => []
    | #"_" :: chrl' => #"\\" :: #"_" :: (replace chrl')
    | x :: chrl'    => x :: (replace chrl')
  in String.implode (replace (String.explode str))
  end

fun writeFile filename content =
    let val fd = TextIO.openOut filename
        val _ = TextIO.output (fd, content) handle e => (TextIO.closeOut fd; raise e)
        val _ = TextIO.closeOut fd
    in () end

fun string_space_complete (len: int) (str: string): string =
  if (String.size str) >= len
  then str
  else string_space_complete len (str ^ "~")

fun clocks_toString (cnt: int) (step_index: int) (clocks: clock list) (clk_number: int) = case clocks of
    [] => ""
  | Clk (clk_name) :: clocks' =>
    let
      val maxlength = largest (List.map (fn Clk cname => (String.size cname)) clocks)
      val str =
        "    \\node at (0.2," ^ (string_of_int cnt) ^ ") (" ^ clk_name ^ ") {\\texttt{" ^ (string_no_subscript (string_space_complete maxlength clk_name)) ^ "}} ;\n"
      ^ "    \\draw[->] (" ^ clk_name ^ ".east) -- +(" ^ (string_of_int step_index) ^ ".5,0) ;\n"
    in str ^ (clocks_toString (cnt - 1) step_index (clocks') clk_number)
    end

fun primitives_toString (clk_pos_assoc: (string * int) list) (G: system) =
  let fun pos_of_clk (cname: string) : int =
    (assoc clk_pos_assoc cname)
  and primitive_toString g = case g of 
      Timestamp (Clk (clk_name), indx, tag) =>
      "    \\node[date] at (" ^ (string_of_int indx) ^ ", " ^ (string_of_int (pos_of_clk clk_name)) ^ ")  {" ^ (minus_instead_of_tild (string_of_tag tag)) ^ "} ;\n"
    | Ticks (Clk (clk_name), indx)          =>
      "    \\node[tick] at (" ^ (string_of_int indx) ^ ", " ^ (string_of_int (pos_of_clk clk_name)) ^ ")  {} ;\n"
    | NotTicks (Clk (clk_name), indx)       =>
      "" (* TODO *)
    | NotTicksUntil (Clk (clk_name), indx)  =>
      "" (* TODO *)
    | NotTicksFrom (Clk (clk_name), indx)   =>
      "" (* TODO *)
    | Affine (t1, t2, t3, t4)               =>
      "" (* TODO *)
  in case G of
     []        => ""
     | g :: G' => (primitive_toString g) ^ (primitives_toString clk_pos_assoc G')
  end


fun instants_labels (n: int) (clk_numbers: int) =
  "    \\foreach \\i/\\x in {" ^ (String.concatWith "," (List.map (fn k => ((string_of_int (k - 1)) ^ "/" ^ (string_of_int k))) (range n))) ^ "} {\n"
(* ^ "      \\node[instant] (I\\i) at (\\x,-" ^ (string_of_int clk_numbers) ^ ".5) {} ; \n" *)
^ "      \\node (I\\i) at (\\x,-" ^ (string_of_int (clk_numbers - 1)) ^ ".5) {} ; \n"
^ "      \\node[instantlab] at (I\\i.south) {\\textsf{\\i}} ;\n"
^ "    }\n"


fun TEX_toString (standalone: bool) (RELEASE_VERSION: string) (step_index: int) (clocks: clock list) (G: system, _, _, _) =
  ""
  ^ "\\begin{tikzpicture}[>=stealth,x=\\xlength,y=\\ylength]\n"
  ^ "    \\foreach \\i in {1,...," ^ (string_of_int step_index) ^ "} {\n"
  ^ "      \\draw[edward, vline] (\\i+.1,-" ^ (string_of_int ((List.length clocks) - 1)) ^ ") -- +(0+.1, " ^ (string_of_int ((List.length clocks) - 1)) ^ ".7) ;\n"
  ^ "    }\n"
  ^ (clocks_toString 0 step_index clocks (List.length clocks))
  ^ (let val x = ref 1 in primitives_toString (List.map (fn Clk cname => (cname, (x := (!x) - 1; !x))) clocks) G end)
  ^ (instants_labels step_index (List.length clocks))
  ^ "\\end{tikzpicture}\n"
