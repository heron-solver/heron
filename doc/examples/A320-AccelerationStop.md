
Airbus A320 Acceleration-Stop Distance
===================
> :wrench: Try me with the latest version of [![GitHub Version](https://img.shields.io/github/release/heron-solver/heron.svg?label=Heron&maxAge=2592000&colorB=46a4b8&style=flat-square)](https://github.com/EmptyStackExn/heron/releases/latest)
> ```
> ./heron --use examples/aviation/A320-AccelerationStop.tesl
> ```

This specification describes an acceleration-stop scenario of the transport-category aircraft Airbus A320. It depicts how physical time and quantities may be expressed by differential equations.

[Airbus Flight Crew Performance Course (A318/A319/A320/A321 Performance Training Manual)](A320-RTOW-PARIS-ORLY-RWY08.jpg):

 - Paris Orly (ORY/LFPO)
 - T/O RWY 08
 - DRY conditions
 - TAILWIND -10 kt
 - WEIGHT 55.8 tons
 - Flaps 1 (CONF 1+F)
 - Maximum takeoff thrust
 - Decision speed: V1 = 118 kt
 - Acceleration-Stop Distance Available: 1500 m

In the following specification, we declare clocks: time in s, speed in m.s⁻¹, speed in kt, and distance in m. Likewise, we declare additional quantity-clocks for the deceleration stage. Note that a clock is a quantity where tags are **monotone**.
```
rational-clock time-S              // in [s]
rational-clock speed-MPS           // in [m.s⁻¹]
rational-clock speed-KT            // in [kt]
rational-quantity distance-M       // in [m]
rational-quantity speed-MPS-DECEL
rational-quantity speed-KT-DECEL
rational-quantity distance-M-DECEL

// Unit conversion between [kt] and [m.s⁻¹]
tag relation speed-KT = <3600/1852> * speed-MPS
tag relation speed-KT-DECEL = <3600/1852> * speed-MPS-DECEL

// V1 threshold
V1-reach sporadic 118.0 on speed-KT
```

We describe how to compute the run distance of the moving object by expressing distance as the differential equation: `dx = v.dt`. Also, we define how speed is gained by uniform acceleration of 4.5 kt/s.
```
// 1. Uniform acceleration of 4.5 kt/s
tag relation speed-KT = 4.5 * time-S

// Compute run distance with time-variant integration: dx = v.dt
tag relation zero = 0.0
tag relation ddistance-M = der distance-M
tag relation dtime-S = der time-S
tag relation speed-MPS_dtime-S = speed-MPS * dtime-S + zero
tag relation ddistance-M = speed-MPS_dtime-S
```

The second part of the specification describes the decelerating process similarly as above. Acceleration or deceleration can be expressed by affine tag relations or also differential equations. To illustrate this, we define that deceleration rate is -3.0 kt/s with the differential equation `dv = ad.dt`:
```
// 2. Uniform deceleration of -3 kt/s with ODE dv = ad.dt
tag relation ad = -3.0
tag relation dspeed-KT-DECEL = der speed-KT-DECEL
tag relation ad-dtime-S = ad * dtime-S + zero
tag relation dspeed-KT-DECEL = ad-dtime-S

// Again, compute run distance when decelerating: dx = v.dt
tag relation ddistance-M-DECEL = der distance-M-DECEL
tag relation speed-MPS_dtime-S-DECEL = speed-MPS-DECEL * dtime-S + zero
tag relation ddistance-M-DECEL = speed-MPS_dtime-S-DECEL
```

Finally, we define our scenario and how the solver unrolls the execution of the specification. We chose to reject takeoff at 20 s and defined a simulation step of 2.5 s:
```
RTO sporadic 20.0 on time-S                      // Reject takeoff at 20 s
time-S periodic 2.5                              // Simulation step
						        
@policy asap                                     // Execute ASAP
@scenario 1 (distance-M -> 0.0)                  // Initial condition of ODE
@run until RTO                                   // Acceleration until RTO
@scenario now (distance-M-DECEL -> distance-M)   // Sync distances
@scenario now (speed-KT-DECEL -> speed-KT)       // Sync speeds
@maxstep 23					        
@run                                             // Deceleration
```

Simulation
----------

<p align="center">
  <img src="A320-AccelerationStop.png">
</p>

The execution trace starts with null time, distance and speed. Acceleration occurs until RTO, where speed and distance are synced with their corresponding clocks for the deceleration part. Finally, the moving object decelerates until approximately 1159 m. Note that distance value is accurate with respect to simulation step size which is here set to 2.5 s. A lower value yields a more accurate computation but slowers the solver.
