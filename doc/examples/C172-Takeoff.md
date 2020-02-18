Cessna C172 Takeoff
===================
> :wrench: Try me with the latest version of [![GitHub Version](https://img.shields.io/github/release/heron-solver/heron.svg?label=Heron&maxAge=2592000&colorB=46a4b8&style=flat-square)](https://github.com/EmptyStackExn/heron/releases/latest)
> ```
> ./heron --use examples/aviation/C172-Takeoff.tesl
> ```

<p align="center">
  <img title="Cessna 172SP Airspeed Indicator. Courtesy of Laminar Research" src="airspeed-indicator.png" width="250">
  <img title="Cessna 172SP Altimeter. Courtesy of Laminar Research" src="altimeter.png" width="250">
  <img title="Cessna 172SP Vertical Speed Indicator. Courtesy of Laminar Research" src="vsi.png" width="250">
</p>

This specification illustrates the TESL language with a simple takeoff scenario. It depicts the computation of quantities and units for time, speed and altitude. Performance speeds (IAS) of a light aircraft such as Cessna 172 are given as:
 - VR = 55 kt
 - Vertical speed in climb: 1200 ft/min

The TESL language allows to define arithmetic relations between clock timeframes. These are called tag relations and allow to describe unit conversions:
```
// Unit conversion between [s] and [min]
tag relation time-S = 60. * time-MIN
// ... and between [kt] and [m.s^-1]
tag relation speed-KT = <3600/1852> * speed-MPS
```

They can also be used to describe a uniform acceleration for the aircraft of 4.5 kt/s by means of an affine relation between `speed-KT` and `time-S`:
```
tag relation speed-KT = 4.5 * time-S
```

The language also allows to describe event-driven and time-driven behaviors. The `sporadic` atom specifies that an event on `VR-reach` will occur when `55.0` is reached on `speed-KT` clock timeframe. Also, synchronous `implies` enforces that if `VR-reach` is triggered, so will `liftoff` instantaneously. In other words, the aircraft lifts off whenever airspeed reaches VR = 55 kt.
```
// Liftoff occurs instantaneously at rotation speed VR = 55 kt
VR-reach sporadic 55.0 on speed-KT
VR-reach implies liftoff
```

To achieve best performance, the pilot-in-command controls climb rate by aiming its target airspeed to Vy. To keep this model simple, we assume in a general physical model that this corresponds to a vertical speed of 1200 ft/min. Again, tag relations serve our purpose:
```
// Climbing with vertical speed of 1200 ft/min
tag relation altitude-FT = 1200.0 * time-MIN + -244.4
```

Finally, flaps retraction must occur when altitude reaches 400 ft. The `time delayed` implication specifies that if `liftoff` occurs, then the event `flaps-retract` will also occur at the instantaneous measured value on `altitude-FT` delayed by `400.0`.
```
liftoff time delayed by 400. on altitude-FT implies flaps-retract
```

Simulation
----------

<p align="center">
  <img src="C172-Takeoff.png" width="450">
</p>

This execution trace depicts a satisfying run starting when time is 0 s. During acceleration at 12 s, speed has reached VR = 55 kt. Consequently, clock `VR-reach` is triggered and hence `liftoff`. The aircraft finally reaches the altitude for flaps retraction at 400 ft.
