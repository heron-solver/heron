(**
   Module CSV

   Author : Hai Nguyen Van
            Université Paris-Saclay / CNRS
   
   The copyright to this code is held by Laboratoire de Recherche en
   Informatique, Université Paris-Sud/CNRS. All rights reserved. This
   file is distributed under the MIT License.
*)

fun writeFile filename content =
    let val fd = TextIO.openOut filename
        val _ = TextIO.output (fd, content) handle e => (TextIO.closeOut fd; raise e)
        val _ = TextIO.closeOut fd
    in () end

fun CSV_toString (step_index: int) (clocks: clock list) (G: context, _, _, _) =
  let val clocks_line =
      (StringMore.concat "," (List.map (fn Clk cname => cname) clocks))
      fun instant_line (step: int) =
	   (StringMore.concat
		 ","
		(List.map
		     (fn clk => case (List.find (fn Timestamp (clk', step', _) => clk = clk' andalso step = step' | _ => false) G) of
				      NONE => ""
				    | SOME (Timestamp (_, _, t)) => tilde_to_minus (string_of_tag t)
				    | _ => "")
		     clocks)
		)
  in clocks_line ^ "\n" ^ (StringMore.concat "\n" (List.map (fn n => instant_line n) (range step_index))) ^ "\n"
  end
