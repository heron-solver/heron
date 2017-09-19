(** Rules used for reduction *)
(* 1. New instant introduction *)
fun ARS_rule_instant_intro
  (G, n, f, []) =
    (G,
     n + 1,
     f @- (SelfModifyingSubs f) @- (ConsumingSubs f) @- (SporadicNowSubs f),
     (ConsumingSubs f) @ (SporadicNowSubs f) @ (ConstantlySubs f) @ (ReproductiveSubs f) @ (SelfModifyingSubs f))
  | ARS_rule_instant_intro _ = raise Assert_failure;

(* 2. Sporadic elimination when deciding to trigger tick sporadicaly *)
fun ARS_rule_sporadic_1
  (G, n, frun, finst) (fsubst as Sporadic (clock, tag)) =
    (G @ [Ticks (clock, n), Timestamp (clock, n, tag)], n, frun, finst @- [fsubst])
  | ARS_rule_sporadic_1 _ _ = raise Assert_failure;

(* 2 Bis. Sporadic elimination when deciding to postpone it *)
fun ARS_rule_sporadic_2
  (G, n, frun, finst) (fsubst as Sporadic (_, _)) =
    (G, n, frun @ [fsubst], finst @- [fsubst])
  | ARS_rule_sporadic_2 _ _ = raise Assert_failure;

(* 3. Implies elimination when premise is false *)
fun ARS_rule_implies_1
  (G, n, frun, finst) (fsubst as Implies (c1, _)) =
    (G @ [NotTicks (c1, n)], n, frun, finst @- [fsubst])
  | ARS_rule_implies_1 _ _ = raise Assert_failure;

(* 4. Implies elimination when premise is true *)
fun ARS_rule_implies_2
  (G, n, frun, finst) (fsubst as Implies (c1, c2)) =
    (G @ [Ticks (c1, n), Ticks (c2, n)], n, frun, finst @- [fsubst])
  | ARS_rule_implies_2 _ _ = raise Assert_failure;

(* 5. Tag relation elimination *)
fun ARS_rule_tagrel_elim
  (G, n, frun, finst) (fsubst as TagRelation (c1, tag1, c2, tag2)) =
    (G @ [Timestamp (c1, n, Schematic (c1, n)), Timestamp (c2, n, Schematic (c2, n)), Affine (Schematic (c1, n), tag1, Schematic (c2, n), tag2)], n, frun, finst @- [fsubst])
  | ARS_rule_tagrel_elim _ _ = raise Assert_failure;

(* 6. Time delayed elimination when premise is false *)
fun ARS_rule_timedelayed_elim_1
  (G, n, frun, finst) (fsubst as TimeDelayedBy (c1, _, _, _)) =
    (G @ [NotTicks (c1, n)], n, frun, finst @- [fsubst])
  | ARS_rule_timedelayed_elim_1 _ _ = raise Assert_failure;

(* 7. Time delayed elimination when premise is true (introduces when-ticking) *)
fun ARS_rule_timedelayed_elim_2
  (G, n, frun, finst) (fsubst as TimeDelayedBy (c1, dt, c2, c3)) =
    (G @ [Ticks (c1, n), Timestamp (c2, n, Schematic (c2, n))], n, frun, (finst @- [fsubst]) @ [WhenTickingOn (c2, Add (Schematic (c2, n), dt), c3)])
  | ARS_rule_timedelayed_elim_2 _ _ = raise Assert_failure;

(* 8. When ticking elimination with merge *)
fun ARS_rule_whentickingon_1
  (G, n, frun, finst) (fsubst as WhenTickingOn (c1, tag, c2)) =
    (G @ [Timestamp (c1, n, tag), Ticks (c2, n)], n, frun, finst @- [fsubst])
  | ARS_rule_whentickingon_1 _ _ = raise Assert_failure;

(* 8 Bis. When ticking elimination postponed *)
fun ARS_rule_whentickingon_2
  (G, n, frun, finst) (fsubst as WhenTickingOn _) =
    (G, n, frun @ [fsubst], finst @- [fsubst])
  | ARS_rule_whentickingon_2 _ _ = raise Assert_failure;

(* 9. Filtered update when false premise *)
fun ARS_rule_filtered_false
  (G, n, frun, finst) (fsubst as FilteredBy (c1, _, _, _, _, _)) =
    (G @ [NotTicks (c1, n)], n, frun @ [fsubst], finst @- [fsubst])
  | ARS_rule_filtered_false _ _ = raise Assert_failure;

(* 10. Filtered update skipping when true premise *)
fun ARS_rule_filtered_update_1
  (G, n, frun, finst) (fsubst as FilteredBy (c1, s, k, rs, rk, c2)) =
    (assert (s > 0 andalso k >= 1);
    (G @ [Ticks (c1, n)], n, frun @ [FilteredBy (c1, s - 1, k, rs, rk, c2)], finst @- [fsubst]))
  | ARS_rule_filtered_update_1 _ _ = raise Assert_failure;

(* 11. Filtered update keeping when true premise *)
fun ARS_rule_filtered_update_2
  (G, n, frun, finst) (fsubst as FilteredBy (c1, s, k, rs, rk, c2)) =
    (assert (s = 0 andalso k > 1);
    (G @ [Ticks (c1, n), Ticks (c2, n)], n, frun @ [FilteredBy (c1, 0, k - 1, rs, rk, c2)], finst @- [fsubst]))
  | ARS_rule_filtered_update_2 _ _ = raise Assert_failure;

(* 12. Filtered update resetting when true premise *)
fun ARS_rule_filtered_update_3
  (G, n, frun, finst) (fsubst as FilteredBy (c1, s, k, rs, rk, c2)) =
    (assert (s = 0 andalso k = 1);
    (G @ [Ticks (c1, n), Ticks (c2, n)], n, frun @ [FilteredBy (c1, rs, rk, rs, rk, c2)], finst @- [fsubst]))
  | ARS_rule_filtered_update_3 _ _ = raise Assert_failure;

(* 13. Delayed elimination when false premise *)
fun ARS_rule_delayed_elim_1
  (G, n, frun, finst) (fsubst as DelayedBy (c1, _, _, _)) =
    (G @ [NotTicks (c1, n)], n, frun, finst @- [fsubst])
  | ARS_rule_delayed_elim_1 _ _ = raise Assert_failure;

(* 14. Delayed elimination when true premise *)
fun ARS_rule_delayed_elim_2
  (G, n, frun, finst) (fsubst as DelayedBy (c1, dp, c2, c3)) =
    (G @ [Ticks (c1, n)], n, frun @ [TimesImpliesOn (c2, dp, c3)], finst @- [fsubst])
  | ARS_rule_delayed_elim_2 _ _ = raise Assert_failure;

(* 15. Times-implies when false premise *)
fun ARS_rule_timesticking_false
  (G, n, frun, finst) (fsubst as TimesImpliesOn (c1, dp, c3)) =
    (G @ [NotTicks (c1, n)], n, frun @ [TimesImpliesOn (c1, dp, c3)], finst @- [fsubst])
  | ARS_rule_timesticking_false _ _ = raise Assert_failure;

(* 16. Times-implies update decrementing when true premise *)
fun ARS_rule_timesticking_update
  (G, n, frun, finst) (fsubst as TimesImpliesOn (c1, dp, c3)) =
    (assert (dp > 1);
    (G @ [Ticks (c1, n)], n, frun @ [TimesImpliesOn (c1, dp - 1, c3)], finst @- [fsubst]))
  | ARS_rule_timesticking_update _ _ = raise Assert_failure;

(* 17. Times-implies update resetting when true premise *)
fun ARS_rule_timesticking_elim
  (G, n, frun, finst) (fsubst as TimesImpliesOn (c1, dp, c2)) =
    (assert (dp = 1);
    (G @ [Ticks (c1, n), Ticks (c2, n)], n, frun, finst @- [fsubst]))
  | ARS_rule_timesticking_elim _ _ = raise Assert_failure;

(* 18. Sustained-from elimination when false start premise *)
fun ARS_rule_sustained_elim_1
  (G, n, frun, finst) (fsubst as SustainedFrom (_, cstart, _, _)) =
    (G @ [NotTicks (cstart, n)], n, frun @ [fsubst], finst @- [fsubst])
  | ARS_rule_sustained_elim_1 _ _ = raise Assert_failure;

(* 19. Sustained-from elimination when true start premise *)
fun ARS_rule_sustained_elim_2
  (G, n, frun, finst) (fsubst as SustainedFrom (c1, cstart, cend, c2)) =
    (G @ [Ticks (cstart, n)], n, frun @ [UntilRestart (c1, c2, cend, cstart)], finst @- [fsubst])
  | ARS_rule_sustained_elim_2 _ _ = raise Assert_failure;

(* 20. Until-restart elimination when false premise *)
fun ARS_rule_untilrestart_elim_1
  (G, n, frun, finst) (fsubst as UntilRestart (c1, _, cend, _)) =
    (G @ [NotTicks (cend, n), NotTicks (c1, n)], n, frun @ [fsubst], finst @- [fsubst])
  | ARS_rule_untilrestart_elim_1 _ _ = raise Assert_failure;

(* 21. Until-restart elimination when true premise *)
fun ARS_rule_untilrestart_elim_2
  (G, n, frun, finst) (fsubst as UntilRestart (c1, c2, cend, _)) =
    (G @ [NotTicks (cend, n), Ticks (c1, n), Ticks (c2, n)], n, frun @ [fsubst], finst @- [fsubst])
  | ARS_rule_untilrestart_elim_2 _ _ = raise Assert_failure;

(* 22. Until-restart elimination restarting when false premise *)
fun ARS_rule_untilrestart_restarts_elim_1
  (G, n, frun, finst) (fsubst as UntilRestart (c1, c2, cend, cstart)) =
    (G @ [Ticks (cend, n), NotTicks (c1, n)], n, frun @ [SustainedFrom (c1, cstart, cend, c2)], finst @- [fsubst])
  | ARS_rule_untilrestart_restarts_elim_1 _ _ = raise Assert_failure;

(* 23. Until-restart elimination restarting when true premise *)
fun ARS_rule_untilrestart_restarts_elim_2
  (G, n, frun, finst) (fsubst as UntilRestart (c1, c2, cend, cstart)) =
    (G @ [Ticks (cend, n), Ticks (c1, n), Ticks (c2, n)], n, frun @ [SustainedFrom (c1, cstart, cend, c2)], finst @- [fsubst])
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
      finst @- [fsubst] @ [Await (Hawait, Hremains @- [hlisten], Hinst @- [hlisten], himp)]) end
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
      finst @- [fsubst] @ [Await (Hawait, Hremains, Hinst @- [hlisten], himp)]) end
  | ARS_rule_await_instant_sigabsent _ _ = raise Assert_failure;

(* 26. Await-remaining instant update when no signal triggered on clock [hlisten] *)
fun ARS_rule_await_next_instant
  (G, n, frun, finst) (fsubst as Await (Hawait, Hremains, Hinst, himp)) =
    let val _ = assert (is_empty Hinst)
    in
     (G,
      n,
      frun @ [Await (Hawait, Hremains, Hremains, himp)],
      finst @- [fsubst]) end
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
      finst @- [fsubst]) end
  | ARS_rule_await_fire _ _ = raise Assert_failure;

(* 28. When-clock implication when premise master clock is false *)
fun ARS_rule_whenclock_implies_1
  (G, n, frun, finst) (fsubst as WhenClock (cmaster, _, _)) =
    (G @ [NotTicks (cmaster, n)], n, frun, finst @- [fsubst])
  | ARS_rule_whenclock_implies_1 _ _ = raise Assert_failure;

(* 29. When-clock implication when premise sampling clock is false*)
fun ARS_rule_whenclock_implies_2
  (G, n, frun, finst) (fsubst as WhenClock (_, csampl, _)) =
    (G @ [NotTicks (csampl, n)], n, frun, finst @- [fsubst])
  | ARS_rule_whenclock_implies_2 _ _ = raise Assert_failure;

(* 30. When-clock implication when premise and conclusion clocks are true *)
fun ARS_rule_whenclock_implies_3
  (G, n, frun, finst) (fsubst as WhenClock (cmaster, csampl, cslave)) =
    (G @ [Ticks (cmaster, n), Ticks (csampl, n), Ticks (cslave, n)], n, frun, finst @- [fsubst])
  | ARS_rule_whenclock_implies_3 _ _ = raise Assert_failure;

(* 31. When-clock implication when premise master clock is false *)
fun ARS_rule_whennotclock_implies_1
  (G, n, frun, finst) (fsubst as WhenNotClock (cmaster, _, _)) =
    (G @ [NotTicks (cmaster, n)], n, frun, finst @- [fsubst])
  | ARS_rule_whennotclock_implies_1 _ _ = raise Assert_failure;

(* 32. When-clock implication when premise clocks are true *)
fun ARS_rule_whennotclock_implies_2
  (G, n, frun, finst) (fsubst as WhenNotClock (cmaster, csampl, _)) =
    (G @ [Ticks (cmaster, n), Ticks (csampl, n)], n, frun, finst @- [fsubst])
  | ARS_rule_whennotclock_implies_2 _ _ = raise Assert_failure;

(* 33. When-clock implication when premise and conclusion clocks are true *)
fun ARS_rule_whennotclock_implies_3
  (G, n, frun, finst) (fsubst as WhenNotClock (cmaster, csampl, cslave)) =
    (G @ [Ticks (cmaster, n), NotTicks (csampl, n), Ticks (cslave, n)], n, frun, finst @- [fsubst])
  | ARS_rule_whennotclock_implies_3 _ _ = raise Assert_failure;

(** SUSTAINED IMMEDIATELY *)
(* 34. Sustained-from-immediately elimination when false start premise *)
fun ARS_rule_sustained_immediately_elim_1
  (G, n, frun, finst) (fsubst as SustainedFromImmediately (_, cstart, _, _)) =
    (G @ [NotTicks (cstart, n)], n, frun @ [fsubst], finst @- [fsubst])
  | ARS_rule_sustained_immediately_elim_1 _ _ = raise Assert_failure;

(* 35. Sustained-from-immediately elimination when true start premise *)
fun ARS_rule_sustained_immediately_elim_2
  (G, n, frun, finst) (fsubst as SustainedFromImmediately (c1, cstart, cend, c2)) =
    (G @ [Ticks (cstart, n)], n, frun, (finst @- [fsubst]) @ [UntilRestartImmediately (c1, c2, cend, cstart)])
  | ARS_rule_sustained_immediately_elim_2 _ _ = raise Assert_failure;

(* 36. Until-restart-immediately elimination when false premise *)
fun ARS_rule_untilrestart_immediately_elim_1
  (G, n, frun, finst) (fsubst as UntilRestartImmediately (c1, _, cend, _)) =
    (G @ [NotTicks (cend, n), NotTicks (c1, n)], n, frun @ [fsubst], finst @- [fsubst])
  | ARS_rule_untilrestart_immediately_elim_1 _ _ = raise Assert_failure;

(* 37. Until-restart-immediately elimination when true premise *)
fun ARS_rule_untilrestart_immediately_elim_2
  (G, n, frun, finst) (fsubst as UntilRestartImmediately (c1, c2, cend, _)) =
    (G @ [NotTicks (cend, n), Ticks (c1, n), Ticks (c2, n)], n, frun @ [fsubst], finst @- [fsubst])
  | ARS_rule_untilrestart_immediately_elim_2 _ _ = raise Assert_failure;

(* 38. Until-restart-immediately elimination restarting when false premise *)
fun ARS_rule_untilrestart_immediately_restarts_elim_1
  (G, n, frun, finst) (fsubst as UntilRestartImmediately (c1, c2, cend, cstart)) =
    (G @ [Ticks (cend, n), NotTicks (c1, n)], n, frun @ [SustainedFromImmediately (c1, cstart, cend, c2)], finst @- [fsubst])
  | ARS_rule_untilrestart_immediately_restarts_elim_1 _ _ = raise Assert_failure;

(* 39. Until-restart-immediately elimination restarting when true premise *)
fun ARS_rule_untilrestart_immediately_restarts_elim_2
  (G, n, frun, finst) (fsubst as UntilRestartImmediately (c1, c2, cend, cstart)) =
    (G @ [Ticks (cend, n), Ticks (c1, n), Ticks (c2, n)], n, frun @ [SustainedFromImmediately (c1, cstart, cend, c2)], finst @- [fsubst])
  | ARS_rule_untilrestart_immediately_restarts_elim_2 _ _ = raise Assert_failure;

(** SUSTAINED WEAKLY *)
(* 40. Sustained-from-weakly elimination when false start premise *)
fun ARS_rule_sustained_weakly_elim_1
  (G, n, frun, finst) (fsubst as SustainedFromWeakly (_, cstart, _, _)) =
    (G @ [NotTicks (cstart, n)], n, frun @ [fsubst], finst @- [fsubst])
  | ARS_rule_sustained_weakly_elim_1 _ _ = raise Assert_failure;

(* 41. Sustained-from-weakly elimination when true start premise *)
fun ARS_rule_sustained_weakly_elim_2
  (G, n, frun, finst) (fsubst as SustainedFromWeakly (c1, cstart, cend, c2)) =
    (G @ [Ticks (cstart, n)], n, frun @ [UntilRestartWeakly (c1, c2, cend, cstart)], finst @- [fsubst])
  | ARS_rule_sustained_weakly_elim_2 _ _ = raise Assert_failure;

(* 42. Until-restart-weakly elimination when false premise *)
fun ARS_rule_untilrestart_weakly_elim_1
  (G, n, frun, finst) (fsubst as UntilRestartWeakly (c1, _, cend, _)) =
    (G @ [NotTicks (cend, n), NotTicks (c1, n)], n, frun @ [fsubst], finst @- [fsubst])
  | ARS_rule_untilrestart_weakly_elim_1 _ _ = raise Assert_failure;

(* 43. Until-restart-weakly elimination when true premise *)
fun ARS_rule_untilrestart_weakly_elim_2
  (G, n, frun, finst) (fsubst as UntilRestartWeakly (c1, c2, cend, _)) =
    (G @ [NotTicks (cend, n), Ticks (c1, n), Ticks (c2, n)], n, frun @ [fsubst], finst @- [fsubst])
  | ARS_rule_untilrestart_weakly_elim_2 _ _ = raise Assert_failure;

(* 44. Until-restart-weakly elimination restarting when false premise *)
fun ARS_rule_untilrestart_weakly_restarts_elim
  (G, n, frun, finst) (fsubst as UntilRestartWeakly (c1, c2, cend, cstart)) =
    (G @ [Ticks (cend, n)], n, frun @ [SustainedFromWeakly (c1, cstart, cend, c2)], finst @- [fsubst])
  | ARS_rule_untilrestart_weakly_restarts_elim _ _ = raise Assert_failure;

(** SUSTAINED IMMEDIATELY WEAKLY *)
(* 45. Sustained-from-immediately-weakly elimination when false start premise *)
fun ARS_rule_sustained_immediately_weakly_elim_1
  (G, n, frun, finst) (fsubst as SustainedFromImmediatelyWeakly (_, cstart, _, _)) =
    (G @ [NotTicks (cstart, n)], n, frun @ [fsubst], finst @- [fsubst])
  | ARS_rule_sustained_immediately_weakly_elim_1 _ _ = raise Assert_failure;

(* 46. Sustained-from-immediately-weakly elimination when true start premise *)
fun ARS_rule_sustained_immediately_weakly_elim_2
  (G, n, frun, finst) (fsubst as SustainedFromImmediatelyWeakly (c1, cstart, cend, c2)) =
    (G @ [Ticks (cstart, n)], n, frun, (finst @- [fsubst]) @ [UntilRestartImmediatelyWeakly (c1, c2, cend, cstart)])
  | ARS_rule_sustained_immediately_weakly_elim_2 _ _ = raise Assert_failure;

(* 47. Until-restart-immediately-weakly elimination when false premise *)
fun ARS_rule_untilrestart_immediately_weakly_elim_1
  (G, n, frun, finst) (fsubst as UntilRestartImmediatelyWeakly (c1, _, cend, _)) =
    (G @ [NotTicks (cend, n), NotTicks (c1, n)], n, frun @ [fsubst], finst @- [fsubst])
  | ARS_rule_untilrestart_immediately_weakly_elim_1 _ _ = raise Assert_failure;

(* 48. Until-restart-immediately-weakly elimination when true premise *)
fun ARS_rule_untilrestart_immediately_weakly_elim_2
  (G, n, frun, finst) (fsubst as UntilRestartImmediatelyWeakly (c1, c2, cend, _)) =
    (G @ [NotTicks (cend, n), Ticks (c1, n), Ticks (c2, n)], n, frun @ [fsubst], finst @- [fsubst])
  | ARS_rule_untilrestart_immediately_weakly_elim_2 _ _ = raise Assert_failure;

(* 49. Until-restart-immediately-weakly elimination restarting when false premise *)
fun ARS_rule_untilrestart_immediately_weakly_restarts_elim
  (G, n, frun, finst) (fsubst as UntilRestartImmediatelyWeakly (c1, c2, cend, cstart)) =
    (G @ [Ticks (cend, n)], n, frun @ [SustainedFromImmediatelyWeakly (c1, cstart, cend, c2)], finst @- [fsubst])
  | ARS_rule_untilrestart_immediately_weakly_restarts_elim _ _ = raise Assert_failure;

(* 50. Immediately delayed elimination when false premise *)
fun ARS_rule_immediately_delayed_elim_1
  (G, n, frun, finst) (fsubst as ImmediatelyDelayedBy (c1, _, _, _)) =
    (G @ [NotTicks (c1, n)], n, frun, finst @- [fsubst])
  | ARS_rule_immediately_delayed_elim_1 _ _ = raise Assert_failure;

(* 51. Immediately delayed elimination when true premise *)
fun ARS_rule_immediately_delayed_elim_2
  (G, n, frun, finst) (fsubst as ImmediatelyDelayedBy (c1, dp, c2, c3)) =
    (G @ [Ticks (c1, n)], n, frun, (finst @- [fsubst]) @ [TimesImpliesOn (c2, dp, c3)])
  | ARS_rule_immediately_delayed_elim_2 _ _ = raise Assert_failure;

(* The lawyer introduces the syntactically-allowed non-deterministic choices that the oracle or the adventurer may decide to use.
   We shall insist that the lawyer only gives pure syntactic possibilities. It is clear those may lead to deadlock and inconsistencies.
   In the next part, we introduce an adventurer which is in charge of testing possibilities and derive configuration until reaching
   the least fixed-point.
*)
fun lawyer_e
  ((_, _, _, finst) : TESL_ARS_conf)
  : (TESL_atomic * (TESL_ARS_conf -> TESL_atomic -> TESL_ARS_conf)) list =
    if finst = []
    then []
    else (* Case where we need to do some paperwork *)
      let
	 (* Major tweak. Due to the orthogonality property of instantaneous solve reduction rules, the order of application of
	    elimination rules does not matter. Hence, we can arbitrarily choose the first atomic psi-formula to reduce, instead of
	    generating useless elim-reduction sequence permutations *)
        val finst = [List.nth (finst, 0)]

        val spors = (List.filter (fn fatom => case fatom of Sporadic _ => true | _ => false) finst)
        val whentickings = (List.filter (fn fatom => case fatom of WhenTickingOn _ => true | _ => false) finst)
        val red_tagrelations = (List.filter (fn fatom => case fatom of TagRelation _ => true | _ => false) finst)
        val red_implies = (List.filter (fn fatom => case fatom of Implies _ => true | _ => false) finst)
        val red_timedelayeds = (List.filter (fn fatom => case fatom of TimeDelayedBy _ => true | _ => false) finst)

        val red_filtereds = (List.filter (fn fatom => case fatom of FilteredBy _ => true | _ => false) finst)
        val red_filtereds_nonneg_s_k = (List.filter (fn fatom => case fatom of FilteredBy (_, s, k, _, _, _) => s > 0 andalso k >= 1 | _ => false) red_filtereds)
        val red_filtereds_noskip = (List.filter (fn fatom => case fatom of FilteredBy (_, s, k, _, _, _) => s = 0 andalso k > 1  | _ => false) red_filtereds)
        val red_filtereds_noskip_nokeep = (List.filter (fn fatom => case fatom of FilteredBy (_, s, k, _, _, _) => s = 0 andalso k = 1 | _ => false) red_filtereds)

        val red_delayeds = (List.filter (fn fatom => case fatom of DelayedBy _ => true | _ => false) finst)

        val red_timesimplies = (List.filter (fn fatom => case fatom of TimesImpliesOn _ => true | _ => false) finst)
        val red_timesimplies_nonneg = (List.filter (fn fatom => case fatom of TimesImpliesOn (_, dp, _) => dp > 1 | _ => false) red_timesimplies)
        val red_timesimplies_now = (List.filter (fn fatom => case fatom of TimesImpliesOn (_, dp, _) => dp = 1 | _ => false) red_timesimplies)

        val red_immediately_delayeds = (List.filter (fn fatom => case fatom of ImmediatelyDelayedBy _ => true | _ => false) finst)

        val red_sustainedfrom = (List.filter (fn fatom => case fatom of SustainedFrom _ => true | _ => false) finst)
        val red_untilrestart = (List.filter (fn fatom => case fatom of UntilRestart _ => true | _ => false) finst)

        val red_sustainedfrom_immediately = (List.filter (fn fatom => case fatom of SustainedFromImmediately _ => true | _ => false) finst)
        val red_untilrestart_immediately = (List.filter (fn fatom => case fatom of UntilRestartImmediately _ => true | _ => false) finst)

        val red_sustainedfrom_weakly = (List.filter (fn fatom => case fatom of SustainedFromWeakly _ => true | _ => false) finst)
        val red_untilrestart_weakly = (List.filter (fn fatom => case fatom of UntilRestartWeakly _ => true | _ => false) finst)

        val red_sustainedfrom_immediately_weakly = (List.filter (fn fatom => case fatom of SustainedFromImmediatelyWeakly _ => true | _ => false) finst)
        val red_untilrestart_immediately_weakly = (List.filter (fn fatom => case fatom of UntilRestartImmediatelyWeakly _ => true | _ => false) finst)

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

         @ (List.map (fn fatom => (fatom, ARS_rule_immediately_delayed_elim_1)) red_immediately_delayeds)
         @ (List.map (fn fatom => (fatom, ARS_rule_immediately_delayed_elim_2)) red_immediately_delayeds)

         @ (List.map (fn fatom => (fatom, ARS_rule_timesticking_false)) red_timesimplies)
         @ (List.map (fn fatom => (fatom, ARS_rule_timesticking_update)) red_timesimplies_nonneg)
         @ (List.map (fn fatom => (fatom, ARS_rule_timesticking_elim)) red_timesimplies_now)

         @ (List.map (fn fatom => (fatom, ARS_rule_sustained_elim_1)) red_sustainedfrom)
         @ (List.map (fn fatom => (fatom, ARS_rule_sustained_elim_2)) red_sustainedfrom)
         @ (List.map (fn fatom => (fatom, ARS_rule_untilrestart_elim_1)) red_untilrestart)
         @ (List.map (fn fatom => (fatom, ARS_rule_untilrestart_elim_2)) red_untilrestart)
         @ (List.map (fn fatom => (fatom, ARS_rule_untilrestart_restarts_elim_1)) red_untilrestart)
         @ (List.map (fn fatom => (fatom, ARS_rule_untilrestart_restarts_elim_2)) red_untilrestart)

         @ (List.map (fn fatom => (fatom, ARS_rule_sustained_immediately_elim_1)) red_sustainedfrom_immediately)
         @ (List.map (fn fatom => (fatom, ARS_rule_sustained_immediately_elim_2)) red_sustainedfrom_immediately)
         @ (List.map (fn fatom => (fatom, ARS_rule_untilrestart_immediately_elim_1)) red_untilrestart_immediately)
         @ (List.map (fn fatom => (fatom, ARS_rule_untilrestart_immediately_elim_2)) red_untilrestart_immediately)
         @ (List.map (fn fatom => (fatom, ARS_rule_untilrestart_immediately_restarts_elim_1)) red_untilrestart_immediately)
         @ (List.map (fn fatom => (fatom, ARS_rule_untilrestart_immediately_restarts_elim_2)) red_untilrestart_immediately)

         @ (List.map (fn fatom => (fatom, ARS_rule_sustained_weakly_elim_1)) red_sustainedfrom_weakly)
         @ (List.map (fn fatom => (fatom, ARS_rule_sustained_weakly_elim_2)) red_sustainedfrom_weakly)
         @ (List.map (fn fatom => (fatom, ARS_rule_untilrestart_weakly_elim_1)) red_untilrestart_weakly)
         @ (List.map (fn fatom => (fatom, ARS_rule_untilrestart_weakly_elim_2)) red_untilrestart_weakly)
         @ (List.map (fn fatom => (fatom, ARS_rule_untilrestart_weakly_restarts_elim)) red_untilrestart_weakly)

         @ (List.map (fn fatom => (fatom, ARS_rule_sustained_immediately_weakly_elim_1)) red_sustainedfrom_immediately_weakly)
         @ (List.map (fn fatom => (fatom, ARS_rule_sustained_immediately_weakly_elim_2)) red_sustainedfrom_immediately_weakly)
         @ (List.map (fn fatom => (fatom, ARS_rule_untilrestart_immediately_weakly_elim_1)) red_untilrestart_immediately_weakly)
         @ (List.map (fn fatom => (fatom, ARS_rule_untilrestart_immediately_weakly_elim_2)) red_untilrestart_immediately_weakly)
         @ (List.map (fn fatom => (fatom, ARS_rule_untilrestart_immediately_weakly_restarts_elim)) red_untilrestart_immediately_weakly)

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

fun new_instant_init (cfs : TESL_ARS_conf list) : TESL_ARS_conf list =
  List.map (ARS_rule_instant_intro) cfs

(*
fun shy_adventurer_step_e (c : TESL_ARS_conf) : TESL_ARS_conf list =
  let val choices = lawyer_e c in
  case choices of
      [] => [c]
    | _  => List.filter (context_SAT) ((List.map (fn (focus, redrule) => redrule c focus) choices))
  end
*)
fun shy_adventurer_step_e (c : TESL_ARS_conf) : TESL_ARS_conf list =
  let val choices = lawyer_e c in
      case choices of
	   [] => [c]
	 | _  => List.foldl
		      (fn ((focus, redrule), l) =>
			   let val cf = redrule c focus
				val cf_reduced = (fn (G, n, phi, psi) => ((* (lfp reduce) *) G, n, phi, psi)) cf in
				if context_SAT cf_reduced
				then cf_reduced :: l
				else l
			   end
		      ) [] choices
  end

fun psi_reduce (last_counter: int) (last_reduced: TESL_ARS_conf list) (pending: TESL_ARS_conf list): TESL_ARS_conf list =
  case pending of
      [] =>
      (writeln "\b\b\b, done.       " ;
	last_reduced)
    | _  =>
      let
	   val reduced = List.concat (List.map (shy_adventurer_step_e) pending)
	   val next_pending = List.filter (fn (_, _, _, psi) => psi <> []) reduced
	   val next_counter = List.length next_pending
	   val next_reduced = List.filter (fn (_, _, _, psi) => psi = []) reduced
	   val _ = print ("\rRemaining universes pending for constraint reduction: " ^ (Int.toString next_counter))
	   val _ = if last_counter < next_counter
		    then print (BOLD_COLOR ^ RED_COLOR ^ " \226\150\178 " ^ RESET_COLOR) (* Or use \226\134\145 *)
		    else print (BOLD_COLOR ^ GREEN_COLOR ^ " \226\150\188 " ^ RESET_COLOR) (* Or use \226\134\147 *)
      in 
	   psi_reduce next_counter (next_reduced @ last_reduced) next_pending
      end

exception Maxstep_reached   of TESL_ARS_conf list;
exception Model_found       of TESL_ARS_conf list;
exception Abort;

fun simplify_whentickings (G: system) (frun: TESL_formula) =
  let
    fun simplify_tag t: tag = case t of
      Unit => t
    | Int _ => t
    | Schematic (clk, n) => (case List.find (fn
              Timestamp(clk', n', Int const) => clk = clk' andalso n = n'
            | Timestamp(clk', n', Rat const) => clk = clk' andalso n = n'
            | _ => false) G of
        NONE => t
      | SOME (Timestamp(_, _, cst)) => cst
      | _ => raise UnexpectedMatch)
    | Add (Int n1, Int n2) => Int (n1 + n2)
    | Rat _ => t
    | Add (Rat x1, Rat x2) => Rat (+/ (x1, x2))
    | Add (t1, t2) => Add (simplify_tag t1, simplify_tag t2)
  in List.map (fn
         WhenTickingOn (c1, tag, c2) => WhenTickingOn (c1, lfp (simplify_tag) tag, c2)
	| f => f
     ) frun
  end

(* Tweak. Given a clock, if a sporadic was chosen to be merged, then it must be the smallest in the specification.
   Otherwise it will surely lead to inconsistencies. Indeed, if time has progressed enough and a past tick is pending
   for merge, the clock of this tick will never react! The tick will be delayed for merge forever, and never merged in
   the future *)
fun policy_no_spurious_sporadics (cfs : TESL_ARS_conf list) : TESL_ARS_conf list =
  List.filter
    (fn (G, _, phi, _) =>
      List.all (fn Sporadic (clk, Int n1) => (List.all (fn Timestamp (clk', _, Int n2) => not (clk = clk') orelse (n2 <= n1) | _ => true) G)
               | Sporadic (clk, Rat q1) => (List.all (fn Timestamp (clk', _, Rat q2) => not (clk = clk') orelse (<=/ (q2, q1)) | _ => true) G)
               | _ => true) phi)
    cfs;
fun policy_no_spurious_whentickings (cfs : TESL_ARS_conf list) : TESL_ARS_conf list =
  List.filter
    (fn (G, _, phi, _) =>
      List.all (fn
        WhenTickingOn (clk, Int n1, _) => (List.all (fn Timestamp (clk', _, Int n2) => not (clk = clk') orelse (n1 >= n2) | _ => true) G)
      | WhenTickingOn (clk, Rat x1, _) => (List.all (fn Timestamp (clk', _, Rat x2) => not (clk = clk') orelse (<=/ (x2, x1)) | _ => true) G)
      | _ => true) phi)
    cfs;


(* Executes exactly one simulation step *)
fun exec_step
  (cfs : TESL_ARS_conf list)
  (step_index: int ref)
  (declared_clocks : clock list)
  (minstep     : int,
   maxstep     : int,
   dumpres     : bool,
   codirection : system,
   heuristics  : TESL_formula
  )
  : TESL_ARS_conf list =
  let
      (* ABORT SIMULATION IF NO REMAINING CONSISTENT SNAPSHOTS *)
      val () = case cfs of
		[] => raise Abort
	     | _  => ()
      val start_time = Time.now()
      (* 1. COMPUTING THE NEXT SIMULATION STEP *)
      val () = writeln (BOLD_COLOR ^ BLUE_COLOR ^ "##### Solve [" ^ string_of_int (!step_index) ^ "] #####" ^ RESET_COLOR)
      val _ = writeln "Initializing new instant..."
      val introduced_cfs = new_instant_init cfs
      val _ = writeln "Preparing constraints..."
      val reduce_psi_formulae = psi_reduce MININT [] introduced_cfs 
      val _ = writeln "Simplifying premodels..."
      val reduced_haa_contexts = List.map (fn (G, n, phi, psi) =>
						    let 
							 val G'   = (lfp reduce) G
							 val phi' = simplify_whentickings G' phi
						    in (G', n, phi', psi)
						    end) reduce_psi_formulae

      (* 2. REMOVE CONFIGURATIONS IN DEADLOCK STATE DUE TO UNMERGEABLE SPORADICS *)
      val no_deadlock = policy_no_spurious_sporadics (policy_no_spurious_whentickings reduced_haa_contexts)

      (* 3. KEEPING PREFIX-COMPLIANT RUNS *)
      val cfs_selected_by_codirection = case codirection of
					    [] => no_deadlock
					   | _	 => (writeln "Keeping prefix-compliant premodels..." ;
						     List.filter (fn (G, _, _, _) => SAT G)
							(List.map (fn (G, n, phi, psi) => (G @ codirection, n, phi, psi)) no_deadlock))

      (* 4. KEEPING HEURISTICS-COMPLIANT RUNS *)
      val cfs_selected_by_heuristic = case heuristics of
	    [] => cfs_selected_by_codirection
	  | _	=> (writeln "Keeping heuristics-compliant premodels..." ;
		       (heuristic_combine heuristics) cfs_selected_by_codirection)

      (* END OF SIMULATION *)
      val end_time = Time.now()
      val _ = step_index := (!step_index) + 1
      val _ = writeln ("--> Consistent premodels: " ^ string_of_int (List.length cfs_selected_by_heuristic))
      val _ = writeln ("--> Step solving time measured: " ^ Time.toString (Time.- (end_time, start_time)) ^ " sec")
      val _ = case cfs_selected_by_heuristic of
		    [] =>
		    (writeln (BOLD_COLOR ^ RED_COLOR ^ "### ERROR: No further state found.") ;
		     writeln ("           Simulation is now stuck in inconsistent mode." ^ RESET_COLOR))
		  | _ => ()  
  in cfs_selected_by_heuristic
  end
  handle
    Abort => (print_dumpres declared_clocks []; [])

(* Solves the specification until reaching a satisfying finite model *)
(* If [maxstep] is -1, then the simulation will be unbounded *)
fun exec
  (cfs : TESL_ARS_conf list)
  (step_index : int ref)
  (declared_clocks : clock list)
  (minstep     : int,
   maxstep     : int,
   dumpres     : bool,
   codirection : system,
   heuristics  : TESL_formula
  )
  : TESL_ARS_conf list =
  let
    val () = writeln "Solving simulation..."
    val () = writeln ("Min. steps: " ^ (if minstep = ~1 then "null" else string_of_int minstep))
    val () = writeln ("Max. steps: " ^ (if maxstep = ~1 then "null" else string_of_int maxstep))
    val () = writeln ("Heuristic: " ^ (case heuristics of [] => "none (full counterfactual exploration)" | _ => List.foldr (fn (DirHeuristic s, s_cur) => s ^ ", " ^ s_cur | _ => raise UnexpectedMatch) "" heuristics))
    (* MAIN SIMULATION LOOP *)
    fun loop cfs =
      let
        (* STOPS WHEN MAXSTEP REACHED *)
        val () =
          if ((!step_index) = maxstep + 1)
          then (writeln ("Stopping simulation at step " ^ string_of_int maxstep ^ " as requested") ;
                writeln (BOLD_COLOR ^ BLUE_COLOR ^ "### End of simulation ###" ^ RESET_COLOR);
		  writeln (BOLD_COLOR ^ YELLOW_COLOR ^ "### WARNING:" ^ RESET_COLOR) ;
                writeln (BOLD_COLOR ^ YELLOW_COLOR ^ "### Solver has returned " ^ string_of_int (List.length cfs) ^ " premodels (partially satisfying and potentially future-spurious models)" ^ RESET_COLOR);
                raise Maxstep_reached cfs)
          else ()
        (* STOPS WHEN FINITE MODEL FOUND *)
        val () =
          let val cfs_sat = List.filter (fn (_, _, frun, _) =>
            (* Stop condition 1. No pending sporadics *)
            (List.length (List.filter (fn fatom => case fatom of Sporadic _ => true | _ => false) frun) = 0)
            (* Stop condition 2. No pending whenticking *)
            andalso (List.length (List.filter (fn fatom => case fatom of WhenTickingOn _ => true | _ => false) frun) = 0)
            (* Stop condition 3. Minstep has already been overheaded *)
            andalso (minstep < (!step_index))
            ) cfs in
          if List.length cfs_sat > 0
          then (writeln ("Stopping simulation when finite model found") ;
                writeln (BOLD_COLOR ^ BLUE_COLOR ^ "### End of simulation ###" ^ RESET_COLOR);
                writeln (BOLD_COLOR ^ GREEN_COLOR ^ "### Solver has successfully returned " ^ string_of_int (List.length cfs_sat) ^ " models" ^ RESET_COLOR);
                raise Model_found cfs_sat)
          else () end
        (* INSTANT SOLVING *)
        val next_snapshots = exec_step cfs step_index declared_clocks (minstep, maxstep, dumpres, codirection, heuristics) in
	 case next_snapshots of
	     [] => []
	   | _ => loop next_snapshots
        end
  in loop cfs end
        handle
	 Maxstep_reached   cfs =>
	 (if dumpres
	  then print_dumpres declared_clocks cfs
	  else writeln "# No output format requested" ;
	  cfs)
        | Model_found       cfs =>
	   (if dumpres
	    then print_dumpres declared_clocks cfs
	    else writeln "# No output format requested" ;
	    cfs)
