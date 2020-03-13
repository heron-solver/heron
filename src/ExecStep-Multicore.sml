(* Executes exactly one simulation step *)
fun exec_step
  (sp: solver_params)
  (cfs : TESL_ARS_conf list)
  : TESL_ARS_conf list =
  let
      val writeln = fn s => (if !(#rtprint sp) then () else (writeln s))
      val write   = fn s => (if !(#rtprint sp) then () else (print s))
      (* ABORT SIMULATION IF NO REMAINING CONSISTENT SNAPSHOTS *)
      val () = case cfs of
		[] => raise Abort
	     | _  => ()
      val start_time = Time.now()

      (* 1. COMPUTING THE NEXT SIMULATION STEP *)
      val () = writeln (BOLD_COLOR ^ BLUE_COLOR ^ "##### Solve [" ^ string_of_int (!(#current_step sp)) ^ "] #####" ^ RESET_COLOR)
      (*  -- 1a. APPLYING INTRODUCTION RULES -- *)
      (*     (Γ, n ⊨ [] ▷ Φ) →i (_, _ ⊨ Φ ▷ Φ) *)
      val _ = write "Initializing new instant..."
      val introduced_cfs = new_instant_init sp cfs
      val _ = clear_line ()
      (*  -- 1b. APPLYING ELIMINATION RULES UNTIL EMPTY PRESENT -- *)
      (*     (Γ, n ⊨ Ψ ▷ Φ) →e ... →e (_, _ ⊨ [] ▷ _) *)
      (*     NB. This is the heaviest part of the solver... Optimization is necessary. *)
      val _ = write "\rPreparing constraints..."
      (* val reduce_psi_formulae = psi_reduce sp introduced_cfs *)
      (* --> Subdivide the problem into 8 tasks to enjoy multicore computation: *)
      val (introduced_cfs_part1, introduced_cfs_part2, introduced_cfs_part3, introduced_cfs_part4, introduced_cfs_part5, introduced_cfs_part6, introduced_cfs_part7, introduced_cfs_part8) =
	   ListMore.split_in_8 introduced_cfs
      val (((reduce_psi_formulae_part1, reduce_psi_formulae_part2),
	     (reduce_psi_formulae_part3, reduce_psi_formulae_part4)),
	    ((reduce_psi_formulae_part5, reduce_psi_formulae_part6),
	     (reduce_psi_formulae_part7, reduce_psi_formulae_part8))) =
	   ForkJoin.par (fn _ => ForkJoin.par (fn _ => ForkJoin.par (fn _ => psi_reduce sp introduced_cfs_part1,
									     fn _ => psi_reduce sp introduced_cfs_part2),
						    fn _ => ForkJoin.par (fn _ => psi_reduce sp introduced_cfs_part3,
									     fn _ => psi_reduce sp introduced_cfs_part4)),
			   fn _ => ForkJoin.par (fn _ => ForkJoin.par (fn _ => psi_reduce sp introduced_cfs_part5,
									     fn _ => psi_reduce sp introduced_cfs_part6),
						    fn _ => ForkJoin.par (fn _ => psi_reduce sp introduced_cfs_part7,
									     fn _ => psi_reduce sp introduced_cfs_part8)))
      val reduce_psi_formulae = reduce_psi_formulae_part1 @ reduce_psi_formulae_part2 @ reduce_psi_formulae_part3 @ reduce_psi_formulae_part4 @ reduce_psi_formulae_part5 @ reduce_psi_formulae_part6 @ reduce_psi_formulae_part7 @ reduce_psi_formulae_part8
      val _ = clear_line ()
      (*  -- 1c. SIMPLIFYING Γ-CONTEXTS -- *)
      val _ = write "\rSimplifying premodels..."
      fun reduce_haa l = List.map (fn (G, n, phi, psi) =>
						    let 
						   (* val G'   = (lfp reduce) G *)
						      val G'   = (lfp (reduce_from (!(#current_step sp) - (pre_depth_formula phi)))) G
						      val phi' = simplify_whentickings G' phi
						    in (G', n, phi', psi)
						    end) l
      (* val reduced_haa_contexts = reduce_haa reduce_psi_formulae *)
      (* --> Subdivide the problem into 8 tasks to enjoy multicore computation: *)
      val (reduce_psi_formulae_part1, reduce_psi_formulae_part2, reduce_psi_formulae_part3, reduce_psi_formulae_part4, reduce_psi_formulae_part5, reduce_psi_formulae_part6, reduce_psi_formulae_part7, reduce_psi_formulae_part8) =
	   ListMore.split_in_8 reduce_psi_formulae

      val (((reduced_haa_contexts_part1, reduced_haa_contexts_part2),
	     (reduced_haa_contexts_part3, reduced_haa_contexts_part4)),
	    ((reduced_haa_contexts_part5, reduced_haa_contexts_part6),
	     (reduced_haa_contexts_part7, reduced_haa_contexts_part8))) =
	   ForkJoin.par (fn _ => ForkJoin.par (fn _ => ForkJoin.par (fn _ => reduce_haa reduce_psi_formulae_part1,
									     fn _ => reduce_haa reduce_psi_formulae_part2),
						    fn _ => ForkJoin.par (fn _ => reduce_haa reduce_psi_formulae_part3,
									     fn _ => reduce_haa reduce_psi_formulae_part4)),
			   fn _ => ForkJoin.par (fn _ => ForkJoin.par (fn _ => reduce_haa reduce_psi_formulae_part5,
									     fn _ => reduce_haa reduce_psi_formulae_part6),
						    fn _ => ForkJoin.par (fn _ => reduce_haa reduce_psi_formulae_part7,
									     fn _ => reduce_haa reduce_psi_formulae_part8)))
      val reduced_haa_contexts = reduced_haa_contexts_part1 @ reduced_haa_contexts_part2 @ reduced_haa_contexts_part3 @ reduced_haa_contexts_part4 @ reduced_haa_contexts_part5 @ reduced_haa_contexts_part6 @ reduced_haa_contexts_part7 @ reduced_haa_contexts_part8

      (* 2. REMOVE CONFIGURATIONS IN DEADLOCK STATE DUE TO UNMERGEABLE SPORADICS *)
      val cfs_no_deadlock = policy_no_spurious_sporadics sp (policy_no_spurious_whentickings sp reduced_haa_contexts)

      (* 3. KEEPING HEURISTICS-COMPLIANT RUNS *)
      val cfs_selected_by_heuristic = case !(#heuristics sp) of
	    [] => cfs_no_deadlock
	  | _	=> (clear_line () ; write "\rKeeping heuristics-compliant premodels..." ;
		       (heuristic_combine (!(#heuristics sp))) cfs_no_deadlock)

      (* END OF SIMULATION *)
      val end_time = Time.now()
      val _ = #current_step sp := !(#current_step sp) + 1
      val _ = clear_line ()
      val _ = writeln ("\r -> Consistent premodels: " ^ string_of_int (List.length cfs_selected_by_heuristic))
      val _ = writeln (" -> Step solving time measured: " ^ Time.toString (Time.- (end_time, start_time)) ^ " s")
      val _ = case cfs_selected_by_heuristic of
		    [] =>
		    (writeln (BOLD_COLOR ^ RED_COLOR ^ "### ERROR: No further state found.") ;
		     writeln ("           Simulation is now stuck in inconsistent mode." ^ RESET_COLOR))
		  | _ => ()  
  in cfs_selected_by_heuristic
  end
  handle
    Abort => (print_dumpres (!(#declared_clocks sp)) []; [])
