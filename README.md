# Circuits.jl

A library for developing circuits at varying levels of abstraction, for various hardware targets.
Circuits may be designed using abstract, hardware-agnostic components; users may provide different implementations
of abstract components for different hardware targets.  This enables users to design circuits at a high-level,
then test different implementations of subcomponents for these circuits, and automatically compile abstract
circuits into different types of hardware without changing the high-level design.

There is not currently much documentation, but `setup.md` outlines the high-level concepts in a (slightly outdated) way.
This library is under development and subject to rapid changes.

See also the `SpikingCircuits.jl` library, which defines the `Spiking` hardware target, the primitive values/components for this target, and a spiking neural network simulator.
