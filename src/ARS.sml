(** Rules used for reduction *)
(* 1. New instant introduction *)
fun ARS_rule_instant_intro
  (G, n, f, []) True =
    (G,
     n + 1,
     @- (@- (@- (f, SelfModifyingSubs f), ConsumingSubs f), SporadicNowSubs f),
     (ConsumingSubs f) @ (SporadicNowSubs f) @ (ConstantlySubs f) @ (ReproductiveSubs f) @ (SelfModifyingSubs f))
  | ARS_rule_instant_intro _ _ = raise Assert_failure;

(* 2. Sporadic elimination when deciding to trigger tick sporadicaly *)
fun ARS_rule_sporadic_1
  (G, n, frun, finst) (fsubst as Sporadic (clock, tag)) =
    (G @ [Ticks (clock, n), Timestamp (clock, n, tag)], n, frun, @- (finst, [fsubst]))
  | ARS_rule_sporadic_1 _ _ = raise Assert_failure;

(* 2 Bis. Sporadic elimination when deciding to postpone it *)
fun ARS_rule_sporadic_2
  (G, n, frun, finst) (fsubst as Sporadic (_, _)) =
    (G, n, frun @ [fsubst], @- (finst, [fsubst]))
  | ARS_rule_sporadic_2 _ _ = raise Assert_failure;

(* 3. Implies elimination when premise is false *)
fun ARS_rule_implies_1
  (G, n, frun, finst) (fsubst as Implies (c1, _)) =
    (G @ [NotTicks (c1, n)], n, frun, @- (finst, [fsubst]))
  | ARS_rule_implies_1 _ _ = raise Assert_failure;

(* 4. Implies elimination when premise is true *)
fun ARS_rule_implies_2
  (G, n, frun, finst) (fsubst as Implies (c1, c2)) =
    (G @ [Ticks (c1, n), Ticks (c2, n)], n, frun, @- (finst, [fsubst]))
  | ARS_rule_implies_2 _ _ = raise Assert_failure;

(* 5. Tag relation elimination *)
fun ARS_rule_tagrel_elim
  (G, n, frun, finst) (fsubst as TagRelation (c1, tag1, c2, tag2)) =
    (G @ [Timestamp (c1, n, Schematic (c1, n)), Timestamp (c2, n, Schematic (c2, n)), Affine (Schematic (c1, n), tag1, Schematic (c2, n), tag2)], n, frun, @- (finst, [fsubst]))
  | ARS_rule_tagrel_elim _ _ = raise Assert_failure;

(* 6. Time delayed elimination when premise is false *)
fun ARS_rule_timedelayed_elim_1
  (G, n, frun, finst) (fsubst as TimeDelayedBy (c1, _, _, _)) =
    (G @ [NotTicks (c1, n)], n, frun, @- (finst, [fsubst]))
  | ARS_rule_timedelayed_elim_1 _ _ = raise Assert_failure;

(* 7. Time delayed elimination when premise is true (introduces when-ticking) *)
fun ARS_rule_timedelayed_elim_2
  (G, n, frun, finst) (fsubst as TimeDelayedBy (c1, dt, c2, c3)) =
    (G @ [Ticks (c1, n), Timestamp (c2, n, Schematic (c2, n))], n, frun @ [WhenTickingOn (c2, Add (Schematic (c2, n), dt), c3)], @- (finst, [fsubst]))
  | ARS_rule_timedelayed_elim_2 _ _ = raise Assert_failure;

(* 8. When ticking elimination with merge *)
fun ARS_rule_whentickingon_1
  (G, n, frun, finst) (fsubst as WhenTickingOn (c1, tag, c2)) =
    (G @ [Timestamp (c1, n, tag), Ticks (c2, n)], n, frun, @- (finst, [fsubst]))
  | ARS_rule_whentickingon_1 _ _ = raise Assert_failure;

(* 8 Bis. When ticking elimination postponed *)
fun ARS_rule_whentickingon_2
  (G, n, frun, finst) (fsubst as WhenTickingOn _) =
    (G, n, frun @ [fsubst], @- (finst, [fsubst]))
  | ARS_rule_whentickingon_2 _ _ = raise Assert_failure;

(* 9. Filtered update when false premise *)
fun ARS_rule_filtered_false
  (G, n, frun, finst) (fsubst as FilteredBy (c1, _, _, _, _, _)) =
    (G @ [NotTicks (c1, n)], n, frun @ [fsubst], @- (finst, [fsubst]))
  | ARS_rule_filtered_false _ _ = raise Assert_failure;

(* 10. Filtered update skipping when true premise *)
fun ARS_rule_filtered_update_1
  (G, n, frun, finst) (fsubst as FilteredBy (c1, s, k, rs, rk, c2)) =
    (assert (s > 0 andalso k > 1);
    (G @ [Ticks (c1, n)], n, frun @ [FilteredBy (c1, s - 1, k, rs, rk, c2)], @- (finst, [fsubst])))
  | ARS_rule_filtered_update_1 _ _ = raise Assert_failure;

(* 11. Filtered update keeping when true premise *)
fun ARS_rule_filtered_update_2
  (G, n, frun, finst) (fsubst as FilteredBy (c1, s, k, rs, rk, c2)) =
    (assert (s = 0 andalso k > 1);
    (G @ [Ticks (c1, n), Ticks (c2, n)], n, frun @ [FilteredBy (c1, 0, k - 1, rs, rk, c2)], @- (finst, [fsubst])))
  | ARS_rule_filtered_update_2 _ _ = raise Assert_failure;

(* 12. Filtered update resetting when true premise *)
fun ARS_rule_filtered_update_3
  (G, n, frun, finst) (fsubst as FilteredBy (c1, s, k, rs, rk, c2)) =
    (assert (s = 0 andalso k = 1);
    (G @ [Ticks (c1, n), Ticks (c2, n)], n, frun @ [FilteredBy (c1, rs, rk, rs, rk, c2)], @- (finst, [fsubst])))
  | ARS_rule_filtered_update_3 _ _ = raise Assert_failure;

(* 13. Delayed elimination when true premise *)
fun ARS_rule_delayed_elim_1
  (G, n, frun, finst) (fsubst as DelayedBy (c1, _, _, _)) =
    (G @ [NotTicks (c1, n)], n, frun, @- (finst, [fsubst]))
  | ARS_rule_delayed_elim_1 _ _ = raise Assert_failure;

(* 14. Delayed elimination when false premise *)
fun ARS_rule_delayed_elim_2
  (G, n, frun, finst) (fsubst as DelayedBy (c1, dp, c2, c3)) =
    (G @ [Ticks (c1, n)], n, frun @ [TimesImpliesOn (c2, dp, c3)], @- (finst, [fsubst]))
  | ARS_rule_delayed_elim_2 _ _ = raise Assert_failure;

(* 15. Times-implies when false premise *)
fun ARS_rule_timesticking_false
  (G, n, frun, finst) (fsubst as TimesImpliesOn (c1, dp, c3)) =
    (G @ [NotTicks (c1, n)], n, frun @ [TimesImpliesOn (c1, dp, c3)], @- (finst, [fsubst]))
  | ARS_rule_timesticking_false _ _ = raise Assert_failure;

(* 16. Times-implies update decrementing when true premise *)
fun ARS_rule_timesticking_update
  (G, n, frun, finst) (fsubst as TimesImpliesOn (c1, dp, c3)) =
    (assert (dp > 1);
    (G @ [Ticks (c1, n)], n, frun @ [TimesImpliesOn (c1, dp - 1, c3)], @- (finst, [fsubst])))
  | ARS_rule_timesticking_update _ _ = raise Assert_failure;

(* 17. Times-implies update resetting when true premise *)
fun ARS_rule_timesticking_elim
  (G, n, frun, finst) (fsubst as TimesImpliesOn (c1, dp, c2)) =
    (assert (dp = 1);
    (G @ [Ticks (c1, n), Ticks (c2, n)], n, frun, @- (finst, [fsubst])))
  | ARS_rule_timesticking_elim _ _ = raise Assert_failure;

(* 18. Sustained-from elimination when false start premise *)
fun ARS_rule_sustained_elim_1
  (G, n, frun, finst) (fsubst as SustainedFrom (_, cstart, _, _)) =
    (G @ [NotTicks (cstart, n)], n, frun @ [fsubst], @- (finst, [fsubst]))
  | ARS_rule_sustained_elim_1 _ _ = raise Assert_failure;

(* 19. Sustained-from elimination when false start premise *)
fun ARS_rule_sustained_elim_2
  (G, n, frun, finst) (fsubst as SustainedFrom (c1, cstart, cend, c2)) =
    (G @ [Ticks (cstart, n)], n, frun @ [UntilRestart (c1, c2, cend, cstart)], @- (finst, [fsubst]))
  | ARS_rule_sustained_elim_2 _ _ = raise Assert_failure;

(* 20. Until-restart elimination when false premise *)
fun ARS_rule_untilrestart_elim_1
  (G, n, frun, finst) (fsubst as UntilRestart (c1, _, cend, _)) =
    (G @ [NotTicks (cend, n), NotTicks (c1, n)], n, frun @ [fsubst], @- (finst, [fsubst]))
  | ARS_rule_untilrestart_elim_1 _ _ = raise Assert_failure;

(* 21. Until-restart elimination when true premise *)
fun ARS_rule_untilrestart_elim_2
  (G, n, frun, finst) (fsubst as UntilRestart (c1, c2, cend, _)) =
    (G @ [NotTicks (cend, n), Ticks (c1, n), Ticks (c2, n)], n, frun @ [fsubst], @- (finst, [fsubst]))
  | ARS_rule_untilrestart_elim_2 _ _ = raise Assert_failure;

(* 22. Until-restart elimination restarting when false premise *)
fun ARS_rule_untilrestart_restarts_elim_1
  (G, n, frun, finst) (fsubst as UntilRestart (c1, c2, cend, cstart)) =
    (G @ [Ticks (cend, n), NotTicks (c1, n)], n, frun @ [SustainedFrom (c1, cstart, cend, c2)], @- (finst, [fsubst]))
  | ARS_rule_untilrestart_restarts_elim_1 _ _ = raise Assert_failure;

(* 23. Until-restart elimination restarting when true premise *)
fun ARS_rule_untilrestart_restarts_elim_2
  (G, n, frun, finst) (fsubst as UntilRestart (c1, c2, cend, cstart)) =
    (G @ [Ticks (cend, n), Ticks (c1, n), Ticks (c2, n)], n, frun @ [SustainedFrom (c1, cstart, cend, c2)], @- (finst, [fsubst]))
  | ARS_rule_untilrestart_restarts_elim_2 _ _ = raise Assert_failure;

(* 24. Await-remaining instant update when caught signal on listening clock [hlisten] *)
fun ARS_rule_await_instant_sigcaught
  (G, n, frun, finst) (fsubst as Await (Hawait, Hremains, Hinst, himp)) =
    let
      val _       = assert (not (is_empty Hinst))
      val hlisten = List.nth (Hinst, 0) 
    in
     (G @ [Ticks (hlisten, n)],
      n,
      frun,
      @- (finst, [fsubst]) @ [Await (Hawait, @- (Hremains, [hlisten]), @- (Hinst, [hlisten]), himp)]) end
  | ARS_rule_await_instant_sigcaught _ _ = raise Assert_failure;

(* 25. Await-remaining instant update when no signal on clock [hlisten] *)
fun ARS_rule_await_instant_sigabsent
  (G, n, frun, finst) (fsubst as Await (Hawait, Hremains, Hinst, himp)) =
    let
      val _       = assert (not (is_empty Hinst))
      val hlisten = List.nth (Hinst, 0) 
    in
     (G @ [NotTicks (hlisten, n)],
      n,
      frun,
      @- (finst, [fsubst]) @ [Await (Hawait, Hremains, @- (Hinst, [hlisten]), himp)]) end
  | ARS_rule_await_instant_sigabsent _ _ = raise Assert_failure;

(* 26. Await-remaining instant update when no signal triggered on clock [hlisten] *)
fun ARS_rule_await_next_instant
  (G, n, frun, finst) (fsubst as Await (Hawait, Hremains, Hinst, himp)) =
    let val _ = assert (is_empty Hinst)
    in
     (G,
      n,
      frun @ [Await (Hawait, Hremains, Hremains, himp)],
      @- (finst, [fsubst])) end
  | ARS_rule_await_next_instant _ _ = raise Assert_failure;

(* 27. Await-remaining instant fire when no remaining signals to wait for *)
fun ARS_rule_await_fire
  (G, n, frun, finst) (fsubst as Await (Hawait, Hremains, Hinst, himp)) =
    let
      val _ = assert (is_empty Hinst)
      val _ = assert (is_empty Hremains)
    in
     (G @ [Ticks (himp, n)],
      n,
      frun @ [Await (Hawait, Hawait, Hawait, himp)],
      @- (finst, [fsubst])) end
  | ARS_rule_await_fire _ _ = raise Assert_failure;

(* 28. When-clock implication when premise master clock is false *)
fun ARS_rule_whenclock_implies_1
  (G, n, frun, finst) (fsubst as WhenClock (cmaster, _, _)) =
    (G @ [NotTicks (cmaster, n)], n, frun, @- (finst, [fsubst]))
  | ARS_rule_whenclock_implies_1 _ _ = raise Assert_failure;

(* 29. When-clock implication when premise sampling clock is false*)
fun ARS_rule_whenclock_implies_2
  (G, n, frun, finst) (fsubst as WhenClock (_, csampl, _)) =
    (G @ [NotTicks (csampl, n)], n, frun, @- (finst, [fsubst]))
  | ARS_rule_whenclock_implies_2 _ _ = raise Assert_failure;

(* 30. When-clock implication when premise and conclusion clocks are true *)
fun ARS_rule_whenclock_implies_3
  (G, n, frun, finst) (fsubst as WhenClock (cmaster, csampl, cslave)) =
    (G @ [Ticks (cmaster, n), Ticks (csampl, n), Ticks (cslave, n)], n, frun, @- (finst, [fsubst]))
  | ARS_rule_whenclock_implies_3 _ _ = raise Assert_failure;

(* 31. When-clock implication when premise master clock is false *)
fun ARS_rule_whennotclock_implies_1
  (G, n, frun, finst) (fsubst as WhenNotClock (cmaster, _, _)) =
    (G @ [NotTicks (cmaster, n)], n, frun, @- (finst, [fsubst]))
  | ARS_rule_whennotclock_implies_1 _ _ = raise Assert_failure;

(* 32. When-clock implication when premise clocks are true *)
fun ARS_rule_whennotclock_implies_2
  (G, n, frun, finst) (fsubst as WhenNotClock (cmaster, csampl, _)) =
    (G @ [Ticks (cmaster, n), Ticks (csampl, n)], n, frun, @- (finst, [fsubst]))
  | ARS_rule_whennotclock_implies_2 _ _ = raise Assert_failure;

(* 33. When-clock implication when premise and conclusion clocks are true *)
fun ARS_rule_whennotclock_implies_3
  (G, n, frun, finst) (fsubst as WhenNotClock (cmaster, csampl, cslave)) =
    (G @ [Ticks (cmaster, n), NotTicks (csampl, n), Ticks (cslave, n)], n, frun, @- (finst, [fsubst]))
  | ARS_rule_whennotclock_implies_3 _ _ = raise Assert_failure;

(* The lawyer introduces the syntactically-allowed non-deterministic choices that the oracle or the adventurer may decide to use.
   We shall insist that the lawyer only gives pure syntactic possibilities. It is clear those may lead to deadlock and inconsistencies.
   In the next part, we introduce an adventurer which is in charge of testing possibilities and derive configuration until reaching
   the least fixed-point.
*)
fun lawyer_i
  ((_, _, _, finst) : TESL_ARS_conf)
  : (TESL_atomic * (TESL_ARS_conf -> TESL_atomic -> TESL_ARS_conf)) list =
  if finst = [] (* No pending red formulae *)
  then [(True, ARS_rule_instant_intro)]
  else []

fun lawyer_e
  ((_, _, _, finst) : TESL_ARS_conf)
  : (TESL_atomic * (TESL_ARS_conf -> TESL_atomic -> TESL_ARS_conf)) list =
    if finst = []
    then []
    else (* Case where we need to do some paperwork *)
      let
	 (* Major tweak. Due to the orthogonality property of instantaneous solve reduction rules, the order of application of
	    elimination rules does not matter. Hence, we can arbitrarily choose the first atomic psi-formula to reduce, instead of
	    generating useless elim-reduction sequence permuatations *)
        val finst = [List.nth (finst, 0)]

        (* Deprecated tweak. We need to decompose here the sporadics in order to avoid space explosion *)
        val spors = (List.filter (fn fatom => case fatom of Sporadic _ => true | _ => false) finst)
(*
        val spors_asap = earliest_sporadics spors
        val spors_later = @- (spors, spors_asap)
*)      
        val whentickings = (List.filter (fn fatom => case fatom of WhenTickingOn _ => true | _ => false) finst)
        val red_tagrelations = (List.filter (fn fatom => case fatom of TagRelation _ => true | _ => false) finst)
        val red_implies = (List.filter (fn fatom => case fatom of Implies _ => true | _ => false) finst)
        val red_timedelayeds = (List.filter (fn fatom => case fatom of TimeDelayedBy _ => true | _ => false) finst)

        val red_filtereds = (List.filter (fn fatom => case fatom of FilteredBy _ => true | _ => false) finst)
        val red_filtereds_nonneg_s_k = (List.filter (fn fatom => case fatom of FilteredBy (_, s, k, _, _, _) => s > 0 andalso k > 1 | _ => false) red_filtereds)
        val red_filtereds_noskip = (List.filter (fn fatom => case fatom of FilteredBy (_, s, k, _, _, _) => s = 0 andalso k > 1  | _ => false) red_filtereds)
        val red_filtereds_noskip_nokeep = (List.filter (fn fatom => case fatom of FilteredBy (_, s, k, _, _, _) => s = 0 andalso k = 1 | _ => false) red_filtereds)

        val red_delayeds = (List.filter (fn fatom => case fatom of DelayedBy _ => true | _ => false) finst)

        val red_timesimplies = (List.filter (fn fatom => case fatom of TimesImpliesOn _ => true | _ => false) finst)
        val red_timesimplies_nonneg = (List.filter (fn fatom => case fatom of TimesImpliesOn (_, dp, _) => dp > 1 | _ => false) red_timesimplies)
        val red_timesimplies_now = (List.filter (fn fatom => case fatom of TimesImpliesOn (_, dp, _) => dp = 1 | _ => false) red_timesimplies)

        val red_sustainedfrom = (List.filter (fn fatom => case fatom of SustainedFrom _ => true | _ => false) finst)
        val red_untilrestart = (List.filter (fn fatom => case fatom of UntilRestart _ => true | _ => false) finst)

        val red_await = (List.filter (fn fatom => case fatom of Await _ => true | _ => false) finst)
        val red_await_norem_noinst = (List.filter (fn fatom => case fatom of Await (_, Hrem, Hinst, _) => is_empty Hrem andalso is_empty Hinst | _ => false) red_await)
        val red_await_rem_noinst   = (List.filter (fn fatom => case fatom of Await (_, Hrem, Hinst, _) => not (is_empty Hrem) andalso is_empty Hinst | _ => false) red_await)
        val red_await_rem_inst     = (List.filter (fn fatom => case fatom of Await (_, Hrem, Hinst, _) => not (is_empty Hrem) andalso not (is_empty Hinst) | _ => false) red_await)

        val red_whenclock = (List.filter (fn fatom => case fatom of WhenClock _ => true | _ => false) finst)
        val red_whennotclock = (List.filter (fn fatom => case fatom of WhenNotClock _ => true | _ => false) finst)

      in   (List.map (fn fatom => (fatom, ARS_rule_sporadic_1)) spors)
         @ (List.map (fn fatom => (fatom, ARS_rule_sporadic_2)) spors)
         @ (List.map (fn fatom => (fatom, ARS_rule_implies_1)) red_implies)
         @ (List.map (fn fatom => (fatom, ARS_rule_implies_2)) red_implies)
         @ (List.map (fn fatom => (fatom, ARS_rule_tagrel_elim)) red_tagrelations)
         @ (List.map (fn fatom => (fatom, ARS_rule_timedelayed_elim_1)) red_timedelayeds)
         @ (List.map (fn fatom => (fatom, ARS_rule_timedelayed_elim_2)) red_timedelayeds)
         @ (List.map (fn fatom => (fatom, ARS_rule_whentickingon_1)) whentickings)
         @ (List.map (fn fatom => (fatom, ARS_rule_whentickingon_2)) whentickings)

         @ (List.map (fn fatom => (fatom, ARS_rule_filtered_false)) red_filtereds)
         @ (List.map (fn fatom => (fatom, ARS_rule_filtered_update_1)) red_filtereds_nonneg_s_k)
         @ (List.map (fn fatom => (fatom, ARS_rule_filtered_update_2)) red_filtereds_noskip)
         @ (List.map (fn fatom => (fatom, ARS_rule_filtered_update_3)) red_filtereds_noskip_nokeep)

         @ (List.map (fn fatom => (fatom, ARS_rule_delayed_elim_1)) red_delayeds)
         @ (List.map (fn fatom => (fatom, ARS_rule_delayed_elim_2)) red_delayeds)

         @ (List.map (fn fatom => (fatom, ARS_rule_timesticking_false)) red_timesimplies)
         @ (List.map (fn fatom => (fatom, ARS_rule_timesticking_update)) red_timesimplies_nonneg)
         @ (List.map (fn fatom => (fatom, ARS_rule_timesticking_elim)) red_timesimplies_now)

         (* TODO: TESTER SUSTAINED ! *)
         @ (List.map (fn fatom => (fatom, ARS_rule_sustained_elim_1)) red_sustainedfrom)
         @ (List.map (fn fatom => (fatom, ARS_rule_sustained_elim_2)) red_sustainedfrom)
         @ (List.map (fn fatom => (fatom, ARS_rule_untilrestart_elim_1)) red_untilrestart)
         @ (List.map (fn fatom => (fatom, ARS_rule_untilrestart_elim_2)) red_untilrestart)
         @ (List.map (fn fatom => (fatom, ARS_rule_untilrestart_restarts_elim_1)) red_untilrestart)
         @ (List.map (fn fatom => (fatom, ARS_rule_untilrestart_restarts_elim_2)) red_untilrestart)

         (* TODO: TESTER AWAIT ! *)
         @ (List.map (fn fatom => (fatom, ARS_rule_await_instant_sigcaught)) red_await_rem_inst)
         @ (List.map (fn fatom => (fatom, ARS_rule_await_instant_sigabsent)) red_await_rem_inst)
         @ (List.map (fn fatom => (fatom, ARS_rule_await_next_instant)) red_await_rem_noinst)
         @ (List.map (fn fatom => (fatom, ARS_rule_await_fire)) red_await_norem_noinst)

         @ (List.map (fn fatom => (fatom, ARS_rule_whenclock_implies_1)) red_whenclock)
         @ (List.map (fn fatom => (fatom, ARS_rule_whenclock_implies_2)) red_whenclock)
         @ (List.map (fn fatom => (fatom, ARS_rule_whenclock_implies_3)) red_whenclock)

         @ (List.map (fn fatom => (fatom, ARS_rule_whennotclock_implies_1)) red_whennotclock)
         @ (List.map (fn fatom => (fatom, ARS_rule_whennotclock_implies_2)) red_whennotclock)
         @ (List.map (fn fatom => (fatom, ARS_rule_whennotclock_implies_3)) red_whennotclock)
      end;

fun shy_adventurer_step_i (c : TESL_ARS_conf) : TESL_ARS_conf list =
  let val choices = lawyer_i c in
  case choices of
      [] => [c]
    | _  => List.filter (context_SAT) ((List.map (fn (focus, redrule) => redrule c focus) choices))
  end

fun shy_adventurer_step_e (c : TESL_ARS_conf) : TESL_ARS_conf list =
  let val choices = lawyer_e c in
  case choices of
      [] => [c]
    | _  => List.filter (context_SAT) ((List.map (fn (focus, redrule) => redrule c focus) choices))
  end

fun psi_reduce (cfs : TESL_ARS_conf list) : TESL_ARS_conf list =
  if (List.all (fn (_, _, _, psi) => psi = []) cfs)
  then cfs
  else psi_reduce (cfl_uniq (List.concat (List.map (shy_adventurer_step_e) cfs)))

fun exec_step (cfs : TESL_ARS_conf list) : TESL_ARS_conf list =
  let
    val introduced_cfs = cfl_uniq (List.concat (List.map (shy_adventurer_step_i) cfs))
    val reduce_psi_formulae = psi_reduce introduced_cfs
    val reduced_haa_contexts = List.map (fn (G, n, phi, psi) => ((lfp reduce) G, n, phi, psi)) reduce_psi_formulae
  in reduced_haa_contexts
  end

exception Maxstep_reached   of TESL_ARS_conf list;
exception Stopclock_reached of TESL_ARS_conf list;
exception Model_found       of TESL_ARS_conf list;

(* Print HAA-system *)
fun print_system (G : system) =
  let
    val G = lfp (reduce) G
    val clocks =
      uniq (List.concat (List.map (fn Ticks (c, _) => [c] | NotTicks (c, _) => [c] | Timestamp (c, _, _) => [c] | Affine _ => []) G))
    val nb_instants =
      List.foldl
        (fn (x, x0) => if x >= x0 then x else x0)
        0
        (List.concat (List.map (fn Ticks (_, n) => [n] | NotTicks (_, n) => [n] | Timestamp (_, n, _) => [n] | Affine _ => []) G))
    val affine_constrs =
      List.filter (fn Affine _ => true | _ => false) G
    val nontriv_timestamps_constrs =
      List.filter (fn Timestamp (_, _, Schematic _) => true | Timestamp (_, _, Add _) => true | _ => false) G
    fun constrs_of_clk_instindex c n =
      List.filter (fn Ticks (c', n') => c = c' andalso n = n' | NotTicks (c', n') => c = c' andalso n = n' | Timestamp (c', n', _) => c = c' andalso n = n' | _ => false) G
    fun string_of_tag (t : tag) = case t of
        Int n => string_of_int n
      | Unit  => "()"
      | Schematic (Clk c_str, n) => "X\226\135\167" ^ (string_of_int n) ^ "\226\135\169" ^ c_str
      | Add (t1, t2) => (string_of_tag t1) ^ " + " ^ (string_of_tag t2)
    fun string_of_timestamp_constr c = case c of
      Timestamp (Clk cname, n, tag) => "X\226\135\167" ^ string_of_int n ^ "\226\135\169" ^ cname ^ " = " ^ string_of_tag tag
    | _ => raise UnexpectedMatch
    fun string_of_affine_constr c = case c of
      Affine (t1, ta, t2, tb) => (string_of_tag t1) ^ " = " ^ (string_of_tag ta) ^ " * " ^  (string_of_tag t2) ^ " + " ^ (string_of_tag tb) | _ => raise UnexpectedMatch
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
      writeln ("[" ^ string_of_int n ^ "]" ^ List.foldl (fn (c, s) => s ^ "\t\t" ^ string_of_constrs_at_clk_instindex c n (constrs_of_clk_instindex c n)) "" clocks)
    fun print_run k =
      if k > nb_instants
      then ()
      else (print_instant k ; print_run (k + 1))
    fun print_affine_contr () =
      (case (affine_constrs, nontriv_timestamps_constrs) of ([], []) => () | _ =>writeln "Affine constraints and non-trivial timestamps:" ;
      List.foldl (fn (c, _) => writeln ("\t" ^ (string_of_affine_constr c))) () affine_constrs ;
      List.foldl (fn (c, _) => writeln ("\t" ^ (string_of_timestamp_constr c))) () nontriv_timestamps_constrs)
  in (writeln "## Simulation result:") ; print_clocks (); print_run 1 ; print_affine_contr () ; (writeln "## End")
end;


(* Solves the specification until reaching a satisfying finite model *)
(* If [maxstep] is -1, then the simulation will be unbounded *)
fun exec
  (cfs : TESL_ARS_conf list)
  (minstep     : int,
   maxstep     : int,
   heuristic   : (TESL_ARS_conf list -> TESL_ARS_conf list) option)
  : TESL_ARS_conf list =
  let
    val () = writeln "Solving simulation..."
    val () = writeln ("Min. steps: " ^ (if minstep = ~1 then "null" else string_of_int minstep))
    val () = writeln ("Max. steps: " ^ (if maxstep = ~1 then "null" else string_of_int maxstep))
    val () = writeln ("Heuristic: " ^ (case heuristic of NONE => "no (full counterfactual exploration)" | SOME _ => "yes"))
    (* MAIN SIMULATION LOOP *)
    fun aux cfs k start_time =
      let
        (* STOPS WHEN FINITE MODEL FOUND *)
        val () =
          let val cfs_sat = List.filter (fn (_, _, frun, _) =>
            (* Stop condition 1. No pending sporadics *)
            (List.length (List.filter (fn fatom => case fatom of Sporadic _ => true | _ => false) frun) = 0)
            (* Stop condition 2. No pending whenticking *)
            andalso (List.length (List.filter (fn fatom => case fatom of WhenTickingOn _ => true | _ => false) frun) = 0)
            (* Stop condition 3. Minstep has already been reached *)
            andalso (minstep < k)
            ) cfs in
          if List.length cfs_sat > 0
          then (writeln ("Stopping simulation when finite model found") ;
                writeln "### End of simulation ###";
                writeln ("### Solver has returned " ^ string_of_int (List.length cfs_sat) ^ " models");
                raise Model_found cfs_sat)
          else () end
        (* STOPS WHEN MAXSTEP REACHED *)
        val () =
          if (k = maxstep + 1)
          then (writeln ("Stopping simulation at step " ^ string_of_int maxstep ^ " as requested") ;
                writeln "### End of simulation ###";
                writeln ("## Solver has returned " ^ string_of_int (List.length cfs) ^ " pre-models (partially satisfying and potentially future-spurious models)\n");
                raise Maxstep_reached cfs)
          else ()
        in let
          (* INSTANT SOLVING *)
          val () = writeln ("##### Solve [" ^ string_of_int k ^ "] #####")
          val cfs' = exec_step cfs
          val end_time = Time.now()
          val cfs_selected_by_heuristic = (case heuristic of NONE => (fn x => x) | SOME h => h) cfs'
          val () = writeln ("--> Consistent pre-models: " ^ string_of_int (List.length cfs_selected_by_heuristic))
          val () = writeln ("--> Step solving time measured: " ^ Time.toString (Time.- (end_time, start_time)) ^ " sec") in
        aux (cfs_selected_by_heuristic) (k + 1) end_time end
        handle Maxstep_reached   cfs => (List.foldl (fn ((G, _, _, _), _) => print_system G) () cfs ; cfs)
              | Model_found       cfs => (List.foldl (fn ((G, _, _, _), _) => print_system G) () cfs ; cfs)
              | Stopclock_reached cfs => cfs
      end
  in aux cfs 1 (Time.now()) end

(* Main solver function *)
fun solve
  (spec : TESL_formula)
  (param : int * int * (TESL_ARS_conf list -> TESL_ARS_conf list) option)
  : TESL_ARS_conf list =
  exec [([], 0, spec, [])] param

(* Heuristic 1. The heuristic is supposed to restrict the universe by choosing configurations that are relevant for simulation *)
(* UNSAFE *)
fun heuristic_minsporadic (cfs : TESL_ARS_conf list) : TESL_ARS_conf list =
  let
    fun min_pending_spor (cfs : TESL_ARS_conf list) : int =
    let
      val cfs_with_spor = List.filter (fn (_, _, frun, _) => List.exists (fn Sporadic _ => true | _ => false) frun) cfs
      val selected_cfs_min_start = case (List.nth (cfs_with_spor, 0)) of (_, _, frunsel, _) =>
        List.length (List.filter (fn Sporadic _ => true | _ => false) frunsel) in
    List.foldl (fn ((_, _, frun, _), n) =>
      let val nb_spor = List.length (List.filter (fn Sporadic _ => true | _ => false) frun) in
      if nb_spor >= n then n else nb_spor end) selected_cfs_min_start cfs_with_spor end
    val min_spor = min_pending_spor cfs
  in List.filter
      (fn (_, _, frun, _) => (List.length (List.filter (fn Sporadic _ => true | _ => false) frun)) <= min_spor + 1) (* TWEAK PARAMETER *)
      cfs end

(* Heuristic 2. Given a clock, if a sporadic was chosen to be merged, then it must be the smallest in the specification.
   Otherwise it will eventually lead to inconsistencies *)
fun heuristic_monotonic_sporadic (cfs : TESL_ARS_conf list) : TESL_ARS_conf list =
  List.filter
    (fn (G, _, phi, _) =>
      let val G = reduce G in
      List.all (fn Sporadic (clk, Int n1) => (List.all (fn Timestamp (clk', _, Int n2) => not (clk = clk') orelse (n1 >= n2) | _ => true) G)
                  | _ => true) phi end)
    cfs;
