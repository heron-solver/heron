(* Rules used for reduction *)
(* 1. New instant introduction *)
fun ARS_rule_instant_intro
  (G, n, f, []) True =
    (G, n + 1, @- (@- (f, SelfModifyingSubs f), ConsumingSubs f), (ConsumingSubs f) @ (ConstantlySubs f) @ (ReproductiveSubs f) @ (SelfModifyingSubs f))
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

(* 8. When ticking elimination *)
fun ARS_rule_whentickingon
  (G, n, frun, finst) (fsubst as WhenTickingOn (c1, tag, c2)) =
    (G @ [Timestamp (c1, n, tag), Ticks (c2, n)], n, @- (frun, [fsubst]), finst)
  | ARS_rule_whentickingon _ _ = raise Assert_failure;

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


(* Returns all combination of systems where at least one clock in [Hstill] is not ticking for the case of await update rule *)
(*
fun await_still_combine (Hstill : clock list) (Himplied : clock) (i : instant_index) : system list =
  (assert (not (is_empty Hstill));
  let
    fun choice_combine Hstill = case Hstill of
        []           => raise Assert_failure
      | [h]          => [[Ticks (h, i)], [NotTicks (h, i)]]
      | h :: Hstill' => let
        val choice_combined = choice_combine Hstill'
        in List.map (fn G => (Ticks (h, i)) :: G) choice_combined
         @ List.map (fn G => (NotTicks (h, i)) :: G) choice_combined
        end
  in List.map
    (fn G => NotTicks (Himplied, i) :: G)
    (@-- ((choice_combine Hstill), [List.map (fn clk => Ticks (clk, i)) Hstill]))
  end);
*)

(* The lawyer introduces the syntactically-allowed non-deterministic choices that the oracle or the adventurer may decide to use.
   We shall insist that the lawyer only gives pure syntactic possibilities. It is clear those may lead to deadlock and inconsistencies.
   In the next part, we introduce an adventurer which is in charge of testing possibilities and derive configuration until reaching
   the least fixed-point.
*)
fun lawyer
  ((_, _, frun, finst) : TESL_ARS_conf)
  : (TESL_atomic * (TESL_ARS_conf -> TESL_atomic -> TESL_ARS_conf)) list =
  if         (List.length (List.filter (fn fatom => case fatom of Sporadic _ => true | _ => false) frun) = 0)      (* No pending sporadics *)
     andalso (List.length (List.filter (fn fatom => case fatom of WhenTickingOn _ => true | _ => false) frun) = 0) (* No pending whenticking *)
     andalso (List.length finst = 0)                                                                               (* No pending red formulae *)
  then []
  else
    if finst = []
    then [(True, ARS_rule_instant_intro)]
    else (* Case where we need to do some paperwork *)
      let
        val spors = (List.filter (fn fatom => case fatom of Sporadic _ => true | _ => false) finst)
        val whentickings = (List.filter (fn fatom => case fatom of WhenTickingOn _ => true | _ => false) frun)
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

      in   (List.map (fn fatom => (fatom, ARS_rule_sporadic_1)) spors)
         @ (List.map (fn fatom => (fatom, ARS_rule_sporadic_2)) spors)
         @ (List.map (fn fatom => (fatom, ARS_rule_implies_1)) red_implies)
         @ (List.map (fn fatom => (fatom, ARS_rule_implies_2)) red_implies)
         @ (List.map (fn fatom => (fatom, ARS_rule_tagrel_elim)) red_tagrelations)
         @ (List.map (fn fatom => (fatom, ARS_rule_timedelayed_elim_1)) red_timedelayeds)
         @ (List.map (fn fatom => (fatom, ARS_rule_timedelayed_elim_2)) red_timedelayeds)
         @ (List.map (fn fatom => (fatom, ARS_rule_whentickingon)) whentickings)

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

      end;

(* The adventurer now explores all paths, cuts those which are halfway inconsistent, and remains at the same spot if no successor exists *)
fun shy_adventurer_step (c : TESL_ARS_conf) : TESL_ARS_conf list =
  let val choices = lawyer c in
  case choices of
      [] => [c]
    | _  => List.filter (context_SAT) ((List.map (fn (focus, redrule) => redrule c focus) choices))
  end

fun adventurer_step (cfs : TESL_ARS_conf list) : TESL_ARS_conf list =
  cfl_uniq (List.concat (List.map (shy_adventurer_step) cfs))

(* Filters all configurations which are in TESL-normal form and whose Presburger-context is SAT. This indicates the adventurer which
   branches that do not require to be further explored as normal form is reached and consistency is checked. *)
fun normal_forms (cfs : TESL_ARS_conf list) : TESL_ARS_conf list =
  List.filter (fn c => is_empty (lawyer c)) cfs;

(* Computes the abstract least fixpoint [abs_lfp] of a functional [ff] starting at [x] until [p] is satified on
   abstract object [abs_lfp]*)
fun lfp_pred_abs (ff: ''a -> ''a) (x: ''a) (abs : ''a -> ''a) (p: ''a -> bool) : ''a =
  let val x' = ff x in
  (if (abs x) = (abs x') andalso (p (abs x)) then (abs x) else lfp_pred_abs (ff) x' (abs) p) end;

(* Computes the least normal forms that can be derived from configuration [c0]. Stops when at least one is found. *)
fun exec
  (c : TESL_ARS_conf)
  : TESL_ARS_conf list =
  lfp_pred_abs (adventurer_step) [c] (normal_forms) (fn abs_cfs => not (is_empty abs_cfs)) ;

(* Warning: A simulation step is different from a reduction step. A simulation step is the transitive-closure of reductions syntactically
   matching empty TESL instant formulae. *)

(* Same as above but stops at most with after [max] execution steps *)
fun exec_maxstep (c: TESL_ARS_conf) (max: int) : TESL_ARS_conf list =
  let
    fun lfp_pred_abs_maxstep (ff: ''a -> ''a) (x: ''a) (abs : ''a -> ''a) (p: ''a -> bool) (cnt: int) : ''a =
      let val x' = ff x in
      (if cnt >= max orelse ((abs x) = (abs x') andalso (p (abs x))) then (abs x) else lfp_pred_abs_maxstep (ff) x' (abs) p (cnt + 1)) end
  in lfp_pred_abs_maxstep (adventurer_step) [c] (normal_forms) (fn abs_cfs => not (is_empty abs_cfs)) 0
end;

(* Same as above but stops at least with [min] execution steps *)
fun exec_minstep (c: TESL_ARS_conf) (min: int) : TESL_ARS_conf list =
  let
    fun lfp_pred_abs_maxstep (ff: ''a -> ''a) (x: ''a) (abs : ''a -> ''a) (p: ''a -> bool) (cnt: int) : ''a =
      let val x' = ff x in
      (if cnt >= min andalso ((abs x) = (abs x') andalso (p (abs x))) then (abs x) else lfp_pred_abs_maxstep (ff) x' (abs) p (cnt + 1)) end
  in lfp_pred_abs_maxstep (adventurer_step) [c] (normal_forms) (fn abs_cfs => not (is_empty abs_cfs)) 0
end;
