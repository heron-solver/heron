exception UnexpectedMatch_Engine_2
(* Solves the specification until reaching a satisfying finite model *)
(* If [maxstep] is -1, then the simulation will be unbounded *)
fun exec
  (sp: solver_params)
  (stop_clks: clock list)
  (cfs : TESL_conf list)
  : TESL_conf list =
  let
    val writeln = (fn s => if !(#rtprint sp) then () else (writeln s))
    val () = writeln "Solving simulation..."
    val () = writeln ("Min. steps: " ^ (if !(#minstep sp) = ~1 then "null" else string_of_int (!(#minstep sp))))
    val () = writeln ("Max. steps: " ^ (if !(#maxstep sp) = ~1 then "null" else string_of_int (!(#maxstep sp))))
    val () = writeln ("Policy: " ^ (case !(#heuristics sp) of [] => "none (exhaustive paths)" | _ => List.foldr (fn (DirHeuristic s, s_cur) => s ^ ", " ^ s_cur | _ => raise UnexpectedMatch_Engine_2) "" (!(#heuristics sp))))
    val () = writeln ("Stop clocks: " ^ (case stop_clks of [] => "null"
									| _ => String.concatWith " " (List.map (fn Clk cname => cname) stop_clks)))
    val next_cfs = ref cfs
    (* MAIN SIMULATION LOOP *)
    val _ = (while (true) do
      let
        val () = if !(#rtprint sp) then print_step_runtime (!(#declared_clocks sp)) (!next_cfs) (!(#current_step sp) - 1) else ()
        (* STOPS WHEN MAXSTEP REACHED *)
        val () =
          if (!(#current_step sp) = !(#maxstep sp) + 1)
          then (writeln ("# Stopping simulation at step " ^ string_of_int (!(#maxstep sp)) ^ " as requested") ;
                writeln (BOLD_COLOR ^ BLUE_COLOR ^ "### End of simulation ###" ^ RESET_COLOR);
		  writeln (BOLD_COLOR ^ YELLOW_COLOR ^ "### WARNING:" ^ RESET_COLOR) ;
                writeln (BOLD_COLOR ^ YELLOW_COLOR ^ "### Solver has returned " ^ string_of_int (List.length (!next_cfs)) ^ (case (!next_cfs) of [] => " premodel" | [_] => " premodel" | _ => " premodels") ^ RESET_COLOR);
                writeln (BOLD_COLOR ^ YELLOW_COLOR ^ "    (partially satisfying and potentially future-spurious models)" ^ RESET_COLOR);
                raise Maxstep_reached (!next_cfs))
          else ()
        (* STOPS WHEN FINITE MODEL FOUND *)
        val () =
          let val cfs_sat = List.filter (fn (_, _, frun, _) =>
            (* Stop condition 1. No pending sporadics *)
            (List.length (List.filter (fn fatom => case fatom of Sporadic _ => true | _ => false) frun) = 0)
            (* Stop condition 2. No pending whenticking *)
            andalso (List.length (List.filter (fn fatom => case fatom of SporadicOn _ => true | _ => false) frun) = 0)
            (* Stop condition 3. Minstep has already been overheaded *)
            andalso (!(#minstep sp) < !(#current_step sp))
            ) (!next_cfs) in
          if List.length cfs_sat > 0
          then (writeln ("# Stopping simulation when finite model found") ;
                writeln (BOLD_COLOR ^ BLUE_COLOR ^ "### End of simulation ###" ^ RESET_COLOR);
                writeln (BOLD_COLOR ^ GREEN_COLOR ^ "### Solver has successfully returned " ^ string_of_int (List.length cfs_sat) ^ (case cfs_sat of [] => " model" | [_] => " model" | _ => " models") ^ RESET_COLOR);
                raise Model_found cfs_sat)
          else () end
	 (* STOPS WHENEVER A CLOCK HAS *EXPLICITLY* TICKED *)
	 val () =
	     let val cfs_sat =
		      List.filter
			   (fn (G, _, _, _) =>
				(List.exists (fn stopc =>
						   (List.exists (fn Ticks (c, _) => c = stopc | _ => false) G)) stop_clks))
			   (!next_cfs)
	     in if List.length cfs_sat > 0
		 then (writeln ("# Stopping simulation as some stop clock has explicitly reacted") ;
			writeln (BOLD_COLOR ^ BLUE_COLOR ^ "### End of simulation ###" ^ RESET_COLOR);
			writeln (BOLD_COLOR ^ YELLOW_COLOR ^ "### Solver has successfully returned " ^ string_of_int (List.length cfs_sat) ^ (case cfs_sat of [] => " model" | [_] => " model" | _ => " models") ^ RESET_COLOR);
                raise Stopclock_ticked (!next_cfs))
		 else () end
        (* INSTANT SOLVING *)
        val _ = next_cfs := exec_step sp (!next_cfs)
	 in case (!next_cfs) of
	     [] => raise EmptySnapshots
	   | _  => ()
      end)
    in (!next_cfs)
  end
  handle
  Maxstep_reached   cfs =>
  (if !(#dumpres sp)
   then print_dumpres (!(#declared_clocks sp)) cfs
   else writeln "# No output format requested" ;
   cfs)
  | Model_found       cfs =>
    (if !(#dumpres sp)
     then print_dumpres (!(#declared_clocks sp)) cfs
     else writeln "# No output format requested" ;
     cfs)
  | Stopclock_ticked       cfs =>
    (if !(#dumpres sp)
     then print_dumpres (!(#declared_clocks sp)) cfs
     else writeln "# No output format requested" ;
     cfs)
  | EmptySnapshots => []
