> A Symbolic Simulation Engine for Timed Causality Models expressed in the [Tagged Events Specification Language](http://wwwdi.supelec.fr/software/TESL/)

This prototype solves specifications given in TESL by exploring full counterfactual facts. In particular, it returns classes of runs modulo non-interfering external components. Such classes are modeled by a Horn-style Affine Arithmetic, for which we embed a decision procedure.

Example
-------------------
	val cf0 : TESL_ARS_conf = ([], 0, [
	  Sporadic (Clk 1, Int 1),
	  Sporadic (Clk 1, Int 2),
	  Implies (Clk 1, Clk 2)], []);
	val cf0_lim = exec cf0;

The execution of the above example produces a run class of size 2

	val cf0_lim =
	   [([Ticks (Clk 1, 1), Timestamp (Clk 1, 1, Int 1), Ticks (Clk 2, 1),
	      Ticks (Clk 1, 2), Timestamp (Clk 1, 2, Int 2), Ticks (Clk 2, 2)],
	2, [Implies (Clk 1, Clk 2)], [])]: TESL_ARS_conf list

References
-------------------
* [TESL: a Language for Reconciling Heterogeneous Execution Traces](http://wwwdi.supelec.fr/fb/publis/2014MemoCODE.pdf), Formal Methods and Models for Codesign (MEMOCODE), 2014 Twelfth ACM/IEEE International Conference on, Lausanne, Switzerland, Oct, 2014, 114-123

Disclaimer
-------------------

Ongoing work. May contain bugs. Not fully tested.

THE PROVIDER MAKES NO REPRESENTATIONS ABOUT THE SUITABILITY, USE, OR PERFORMANCE OF THIS PROGRAM OR ABOUT ANY CONTENT OR INFORMATION MADE ACCESSIBLE BY THESE, FOR ANY PURPOSE.
