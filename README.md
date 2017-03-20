Heron
===================

> Interactive Simulation Solver for Timed Causality Models expressed in TESL

[![Build Status](https://travis-ci.org/EmptyStackExn/heron.svg?branch=master)](https://travis-ci.org/EmptyStackExn/heron)

**Heron** is a solver for the [Tagged Events Specification Language (TESL)](http://wwwdi.supelec.fr/software/TESL/), a declarative language to specify synchronization of discrete events for simulation. This prototype provides:

 1. Definition of TESL specifications for causality and time scale between events.
 2. Simulation by full or partial generation of execution traces.
 3. Runtime compliance-monitoring of systems (SUT).


Getting started
-------------------
We recommend compiling with [MLton](http://mlton.org/) to enjoy code optimization features. To build sources on Ubuntu, type the following
```bash
sudo apt-get install mlton ml-lex ml-yacc
make
```

Several examples are provided in `examples` directory. To solve one of them, you can simply type
```bash
./heron < examples/basic/FirstExample.tesl
```

Example
-------------------
The following specification is available in `ImplicationsTimeScales.tesl` and [detailed here](http://wwwdi.supelec.fr/software/TESL/#Implications).

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

This tool uses *counterfactual reasoning* on the purely-syntactic structure of TESL-formulae to derive disjunction cases. Such execution branches are iteratively abstracted as context constraints, which refine the desired behavior. Along the simulation, inconsistent branches are filtered out by an arithmetic decision procedure.

By default, the solver stops whenever a *finite satisfying run* is found. The simulation of the above example converges at the 5th step and returns 32 possible behaviors:
```
### Solver has successfully returned 32 models
## Simulation result:
		m1		m2		slave		
[1]		⇑ 1		⊘		⇑
[2]		⊘		⇑ 2		⇑
[3]		⇑ 4		⊘		⇑
[4]		⊘		⇑ 5		⇑
[5]		⇑ 7		⊘		⇑
## End
...
```

References
-------------------

 1. [TESL: a Language for Reconciling Heterogeneous Execution Traces](https://ieeexplore.ieee.org/document/6961849), Formal Methods and Models for Codesign (MEMOCODE), 2014 Twelfth ACM/IEEE International Conference on, Lausanne, Switzerland, Oct, 2014, 114-123
 2. The project is named after [Heron of Alexandria](http://www-history.mcs.st-andrews.ac.uk/Biographies/Heron.html), the first-century Greek mathematician and engineer.

License
-------------------

Heron is released under the MIT License.

THE PROVIDER MAKES NO REPRESENTATIONS ABOUT THE SUITABILITY, USE, OR PERFORMANCE OF THIS SOFTWARE OR ABOUT ANY CONTENT OR INFORMATION MADE ACCESSIBLE BY THE SOFTWARE, FOR ANY PURPOSE. THE SOFTWARE IS PROVIDED "AS IS," WITHOUT EXPRESS OR IMPLIED WARRANTIES INCLUDING, BUT NOT LIMITED TO, ANY IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, OR NONINFRINGEMENT WITH RESPECT TO THE SOFTWARE. THE PROVIDER IS NOT OBLIGATED TO SUPPORT OR ISSUE UPDATES TO THE SOFTWARE.
