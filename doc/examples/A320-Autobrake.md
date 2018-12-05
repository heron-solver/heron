Airbus A320 Autobrake System
===================
:wrench: Try me with the latest version [![GitHub Version](https://img.shields.io/github/release/heron-solver/heron.svg?label=Heron&maxAge=2592000&colorB=46a4b8&style=flat-square)](https://github.com/EmptyStackExn/heron/releases/latest)
```
./heron --use examples/aviation/A320-Autobrake.tesl
```

The following specification describes a takeoff scenario of the transport-category aircraft Airbus A320. The default scenario describes a normal takeoff with no engine failure. It illustrates the purpose of mixing event-driven and time-driven behaviors.

[Airbus Flight Crew Performance Course (A318/A319/A320/A321 Performance Training Manual)](A320-RTOW-PARIS-ORLY-RWY08.jpg):

 - Paris Orly (ORY/LFPO)
 - T/O RWY 08
 - DRY conditions
 - TAILWIND -10 kt
 - WEIGHT 55.8 tons
 - Flaps 1 (CONF 1+F)
 - Maximum takeoff thrust

Performance speeds (IAS):
 - Decisions speed: V1 = 118 kt
 - Rotate speed: VR = 126 kt

In the following specification, we express three clocks `time-S`, `speed-MPS` and `speed-KT` which describe physical time and quantities for the case study. A tag relation is described to define unit conversions between m.s⁻¹ and kt, and uniform acceleration is defined by 4.5 kt/s. We define a race condition between brake application and reaching V1 speed.
```
rational-clock time-S     // in [s]
rational-clock speed-MPS  // in [m.s^-1]
rational-clock speed-KT   // in [kt]

// Unit conversion between [kt] and [m.s^-1]
tag relation speed-KT = <3600/1852> * speed-MPS

// Uniform acceleration of 4.5 kt/s
tag relation speed-KT = 4.5 * time-S

// Speed thresholds
V1-reach strictly precedes VR-reach
V1-reach sporadic 118.0 on speed-KT
VR-reach sporadic 126.0 on speed-KT

// Liftoff occurs 3s after reaching VR
VR-reach time delayed by 3. on time-S implies liftoff
```

Autobrake activates whenever it was previously armed along with ground spoilers, and airspeed has exceeded 72 kt. Should takeoff be rejected (clock `RTO`), brakes will be applied `BRK-apply` and V1 will not be reached in the future. Otherwise, reaching V1 prevents from rejecting takeoff.
```
// Autobreak
SPLR-arm strictly precedes AUTOBRK-on
AUTOBRK-arm strictly precedes AUTOBRK-on
AUTOBRK-on sporadic 72.0 on speed-KT

// In the event of takeoff rejection (RTO), the system applies brakes
// as long as autobrakes were in active state
RTO sustained from AUTOBRK-on to AUTOBRK-off implies BRK-apply

// RTO is forbidden after reaching V1
// Conversely, applying brakes prevents from reaching takeoff speeds
V1-reach kills RTO
BRK-apply kills V1-reach
```

Go Situation
----------

<img src="A320-Autobrake-TO.png" width="800">

In the first simulation, takeoff occurs normally. In the first instant, ground spoilers and autobrake were armed. Acceleration begins. At 72 kt, autobrake becomes ready. Decision of whether continuing or aborting takeoff by the pilot-in-command occurs at 118 kt defines the V1 speed limit. Wheels rotates at VR = 126 kt and the aircraft is airborne 3 s after (liftoff).

No Go Situation
----------

<img src="A320-Autobrake-RTO-after72.png" width="800">

An alternative satisfying run is possible when adding `RTO sporadic 25. on time-S` which specifies to reject takeoff at 25 s. In this case, it is not possible to reach V1, VR and hence aircraft lift off.
