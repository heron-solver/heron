> A Symbolic Simulation Engine for Timed Causality Models expressed in the [Tagged Events Specification Language (TESL)](http://wwwdi.supelec.fr/software/TESL/)

This prototype solves specifications given in TESL by exploring full counterfactual facts. In particular, it returns classes of runs modulo non-interfering external components. Such classes are modeled by a Horn-style Affine Arithmetic, for which we embed a decision procedure.

Example
-------------------
```
int-clock master1 sporadic 1, 4, 7
int-clock master2 sporadic   2, 5
unit-clock slave
tag relation master1 = master2

master1 implies slave
master2 implies slave
```

The TESL tagged event engine will produce the following run:
![Example 3, TESL Official Website](http://wwwdi.supelec.fr/software/downloads/TESL/example3.svg)

Our tool is able explores counterfactual facts all stops wheneven a finite satisfying run is found
```
## Simulation result:
		m1		slave		m2		
[1]		⇑ 1		⇑		  1
[2]		🛇				⇑ 2
[3]		⇑ 4		⇑		  4
[4]		🛇				⇑ 5
[5]		⇑ 7		⇑		  7
## End
## Simulation result:
		m1		slave		m2		
[1]		⇑ 1		⇑		  1
[2]		🛇				⇑ 2
[3]		⇑ 4		⇑		  4
[4]		⇑ 5		⇑		⇑ 5
[5]		⇑ 7		⇑		  7
## End
## Simulation result:
		m1		slave		m2		
[1]		⇑ 1		⇑		  1
[2]		⇑ 2		⇑		⇑ 2
[3]		⇑ 4		⇑		  4
[4]		🛇				⇑ 5
[5]		⇑ 7		⇑		  7
## End
## Simulation result:
		m1		slave		m2		
[1]		⇑ 1		⇑		  1
[2]		⇑ 2		⇑		⇑ 2
[3]		⇑ 4		⇑		  4
[4]		⇑ 5		⇑		⇑ 5
[5]		⇑ 7		⇑		  7
## End
```

Getting started
-------------------
You can use this tool with different compilers as it is written in Standard ML. It is recommended to use [MLton compiler](http://mlton.org/) as it provides interesting code optimization features. To run the above example in `Examples.sml`, just type
```
make run
```

If you decide to use [Poly/ML](http://www.polyml.org/) instead, you need `polybuild`:
```
mkdir contrib
hg clone --cwd contrib https://bitbucket.org/cannam/sml-buildscripts
```

References
-------------------
* [TESL: a Language for Reconciling Heterogeneous Execution Traces](http://wwwdi.supelec.fr/fb/publis/2014MemoCODE.pdf), Formal Methods and Models for Codesign (MEMOCODE), 2014 Twelfth ACM/IEEE International Conference on, Lausanne, Switzerland, Oct, 2014, 114-123

Disclaimer
-------------------

Ongoing work. May contain bugs. Not fully tested.

THE PROVIDER MAKES NO REPRESENTATIONS ABOUT THE SUITABILITY, USE, OR PERFORMANCE OF THIS PROGRAM OR ABOUT ANY CONTENT OR INFORMATION MADE ACCESSIBLE BY THESE, FOR ANY PURPOSE.

