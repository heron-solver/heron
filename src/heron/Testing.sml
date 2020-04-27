(* Adds and solves a scenario with parameters [(strictness, step_index, tclks)] *)
fun scenario_add
  (sp: solver_params)
  (strictness, step_index, tclks)
  (snapshots : solver_state)
  : unit =
    let val scenario: context ref = ref []
        val n = (case step_index of NowPos  => if !(#current_step sp) = 1 then 1 else !(#current_step sp) - 1
				      | NextPos => !(#current_step sp)
				      | Pos n   => n)
	 val () = writeln (BOLD_COLOR ^ MAGENTA_COLOR ^ "### Scenario [" ^ (string_of_int n) ^ "] ###" ^ RESET_COLOR)
	 val _ = List.app (fn (c, otag) =>
				 (scenario <>> (Ticks (c, n)) ;
				  case otag of NoneTag    => ()
					      | CstTag tag => scenario <>> (Timestamp (c, n, tag))
					      | SymbTag c_to_retrieve => (scenario <>> (Timestamp (c, n, Schematic (c, n))) ;
									      scenario <>> (AffineRefl (Schematic (c, n), Schematic (c_to_retrieve, n))))
			    )) tclks
	 val _ = if strictness
		  then List.app (fn c => if List.exists (fn (x, _) => x = c) tclks
					    then ()
					    else scenario <>> NotTicks (c, n)) (!(#declared_clocks sp))
		  else ()
	 val start_time = Time.now()
	 val () = snapshots := List.map (fn (G, n, phi, psi) => (G @ (!scenario), n, phi, psi)) (!snapshots)
	 val () = snapshots := List.map (fn (G, n, phi, psi) =>
						 let 
						   val G'   = (lfp reduce) G
						   val phi' = simplify_whentickings G' phi
						 in (G', n, phi', psi)
						 end) (!snapshots)
	 val () = snapshots := List.filter (fn (G, _, _, _) => SAT (!(#declared_quantities sp)) G) (!snapshots)
	 val end_time = Time.now()
	 val _ = clear_line ()
	 val _ = writeln ("\r -> Consistent premodels: " ^ string_of_int (List.length (!snapshots)))
	 val _ = writeln (" -> Step solving time measured: " ^ Time.toString (Time.- (end_time, start_time)) ^ " s")
	 val _ = case (!snapshots) of
		      [] =>
		      (writeln (BOLD_COLOR ^ RED_COLOR ^ "### ERROR: No further state found.") ;
			writeln ("           Simulation is now stuck in inconsistent mode." ^ RESET_COLOR))
		    | _ => ()  
				 
    in ()
    end

(* Event concretization is necessary for I/O test generation *)
(* BETA: Using bounded random *)
fun event_concretize
  (sp: solver_params)
  index
  snapshots =
  let
    fun concretize_cf (G, current, phi, _) dclk i = let
      (* Concretizing ticking predicate *)
      val G =
	 if List.exists (fn Ticks (clk, i') => clk = dclk andalso i' = i | _ => false) G
	 then G
	 else NotTicks (dclk, i) :: G (* OR SHOULD IT BE RANDOM TOO? *)
      (* Concretizing tag variable *)
      val seed = valOf (MLton.Random.useed ())
      val G = if List.exists (fn Timestamp (clk, i', _) => clk = dclk andalso i' = i | _ => false) G
		then G
		else case clk_type_lookup (!(#clock_types sp)) dclk of
		    Unit_t => Timestamp (dclk, i, Unit) :: G
		  | Int_t => let 
		    val inf_bound = max_list (::<=)
						 (List.map
						      (fn Timestamp (_, _, tag) => tag | _ => raise UnexpectedMatch)
						      (List.filter (fn Timestamp (clk, i', _) => clk = dclk andalso i' < i | _ => false) G))
		    val sup_bound = min_list (::<=)
						 (List.map
						      (fn Timestamp (_, _, tag) => tag | _ => raise UnexpectedMatch)
						      (List.filter (fn Timestamp (clk, i', _) => clk = dclk andalso i' > i | _ => false) G))
		    val delta = 10
		    (* HERE COMES THE MAGIC *)
		    val chosen_tag = case (inf_bound, sup_bound) of
		        (NONE, NONE)                     => Int (random_int_range (0, delta) seed)
		      | (SOME (Int inf), NONE)           => Int (random_int_range (inf, inf + delta) seed)
		      | (NONE, SOME (Int sup))           => Int (random_int_range (sup - delta, sup) seed)
		      | (SOME (Int inf), SOME (Int sup)) => Int (random_int_range (inf, sup) seed)
		      | _                                => raise UnexpectedMatch
		  in Timestamp (dclk, i, chosen_tag) :: G
		  end
		  | Rat_t => let 
		    val inf_bound = max_list (::<=)
						 (List.map
						      (fn Timestamp (_, _, tag) => tag | _ => raise UnexpectedMatch)
						      (List.filter (fn Timestamp (clk, i', _) => clk = dclk andalso i' < i | _ => false) G))
		    val sup_bound = min_list (::<=)
						 (List.map
						      (fn Timestamp (_, _, tag) => tag | _ => raise UnexpectedMatch)
						      (List.filter (fn Timestamp (clk, i', _) => clk = dclk andalso i' > i | _ => false) G))
		    val rdelta = */ (random_rat seed, rat_make (10, 1)) (* Rational number between [0.0, 10.0] *)
		    val chosen_tag = case (inf_bound, sup_bound) of
		        (NONE, NONE)                     => Rat (rdelta)
		      | (SOME (Rat inf), NONE)	      => Rat (+/ (inf, rdelta))
		      | (NONE, SOME (Rat sup))	      => Rat (-/ (sup, rdelta))
		      | (SOME (Rat inf), SOME (Rat sup)) => Rat (+/ (inf, */ (random_rat seed, -/(sup,inf))))
		      | _                                => raise UnexpectedMatch
		  in Timestamp (dclk, i, chosen_tag) :: G
		  end
    in ((* POTENTIAL BUG *) reduce G, current, phi, [])
    end
  in case index of
	  (* Concretize all snapshots*)
	  NONE   =>
	  List.map (fn cf => List.foldl (fn (dclk, (G, current_step, phi, _)) =>
						 List.foldl (fn (k, cf) => concretize_cf cf dclk k) (G, current_step, phi, []) (range current_step)
					    ) cf (!(#declared_clocks_driving sp))) snapshots
	  (* Concretize [index]-th snapshot *)
	| SOME _ =>
	  List.map (fn cf => List.foldl (fn (dclk, cf) =>
						 concretize_cf cf dclk (valOf index)
					    ) cf (!(#declared_clocks_driving sp))) snapshots
  end
