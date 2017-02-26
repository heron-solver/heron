Heron
===================

[![Build Status](https://travis-ci.org/EmptyStackExn/heron.svg?branch=master)](https://travis-ci.org/EmptyStackExn/heron)

> Symbolic Simulation Engine for Timed Causality Models expressed in the [Tagged Events Specification Language (TESL)](http://wwwdi.supelec.fr/software/TESL/)

This prototype solves specifications given in TESL by exploring full counterfactual facts. In particular, it returns classes of runs modulo non-interfering external components. Such classes are modeled in Horn-style Affine Arithmetic, for which we embed a decision procedure.

The project is named after [Heron of Alexandria](http://www-history.mcs.st-andrews.ac.uk/Biographies/Heron.html), the first century Greek mathematician and engineer.

Getting started
-------------------
We recommend to compile with [MLton](http://mlton.org/) as it provides interesting code optimization features in Standard ML. To build sources, just type
```bash
make
```

Several examples are provided in `examples` directory. To execute one of them, you can simply type
```bash
./heron < examples/gallery/LightSwitch.tesl
```

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

Our tool is able to explore counterfactual facts and stops whenever a finite satisfying run is found
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

References
-------------------
* [TESL: a Language for Reconciling Heterogeneous Execution Traces](https://ieeexplore.ieee.org/document/6961849), Formal Methods and Models for Codesign (MEMOCODE), 2014 Twelfth ACM/IEEE International Conference on, Lausanne, Switzerland, Oct, 2014, 114-123

Disclaimer
-------------------

All rights reserved.

THE PROVIDER MAKES NO REPRESENTATIONS ABOUT THE SUITABILITY, USE, OR PERFORMANCE OF THIS SOFTWARE OR ABOUT ANY CONTENT OR INFORMATION MADE ACCESSIBLE BY THE SOFTWARE, FOR ANY PURPOSE. THE SOFTWARE IS PROVIDED "AS IS," WITHOUT EXPRESS OR IMPLIED WARRANTIES INCLUDING, BUT NOT LIMITED TO, ANY IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, OR NONINFRINGEMENT WITH RESPECT TO THE SOFTWARE. THE PROVIDER IS NOT OBLIGATED TO SUPPORT OR ISSUE UPDATES TO THE SOFTWARE.
