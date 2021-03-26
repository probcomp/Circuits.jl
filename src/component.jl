using LightGraphs

######################
# Abstract interface #
######################

"""
    abstract type Component end

A circuit component.
"""
abstract type Component end

"""
   abstract type GenericComponent <: Component end

A component representation which is not primitive for a target, nor composed
of other components (though it may be able to be _implemented_ using other components).
"""
abstract type GenericComponent <: Component end

"""
    abstract type PrimitiveComponent{T} <: Component end

A component primitive to target `T <: Target`.  Ie. the simulator for that target
can operate directly on this component.
"""
abstract type PrimitiveComponent{Target} <: Component end

"""
    inputs(::Component)::CompositeValue

A `CompositeValue` giving the inputs to this component.
"""
inputs(::Component)::CompositeValue = error("Not implemented.")

"""
    outputs(::Component)::CompositeValue

A `CompositeValue` giving the outputs from this component.
"""
outputs(::Component)::CompositeValue = error("Not implemented.")

"""
    abstract(c::Component)

The more abstract component which was implemented to yield `c`, or `nothing` if such a component does
not exist or is not available.
"""
abstract(::Component) = nothing

"""
    target(c::Component)

If `c` is a _concrete component_ which has 1 implementation for 1 target,
returns the `Target` `c` can be implemented for.  Else, returns `nothing`.
"""
target(::Component) = error("Not implemented.  (This may be a non-concrete value with multiple possible targets.)")

"""
    implement(c::Component, t::Target)

Make progress implementing the component for the target, so that a finite
number of repeated calls to `implement` will yield a component `c` so that
`is_implementation_for(c, t)` is true.        
"""
implement(c::Component, t::Target) = no_impl_error(c, t)
implement(c::PrimitiveComponent{T1}, t::T2) where {T1 <: Target, T2 <: Target} =
    if T1 <: T2
        c
    else
        error("Cannot implement $c, a PrimitiveComponent{$T1}, for target $t.")
    end

"""
    is_implementation_for(::Component, ::Target)

Whether the given component is an implementation for the given target (ie. whether it can be simulated
by the simulator for that target without any additional implementation work).
"""
is_implementation_for(::PrimitiveComponent{<:T}, ::T) where {T <: Target} = true
is_implementation_for(::PrimitiveComponent, ::Target) = false
is_implementation_for(::GenericComponent, ::Target) = false

"""
    implement_deep(::Component, t::Target)

Implement the component for the target recursively, yielding a component `c` such that
`is_implementation_for(c, t)` is true.
"""
implement_deep(c::PrimitiveComponent{T1}, t::T2) where {T1 <: Target, T2 <: Target} =
    if T1 <: T2
        c
    else
        error("Cannot implement $c, a PrimitiveComponent{$T1}, for target $t.")
    end
implement_deep(c::GenericComponent, t::Target) = implement_deep(implement(c, t), t)

### CompositeComponent ###

include("composite_component.jl")

### ComponentGroup ###

# TODO: better documentation
# TODO: could I directly have this be a subtype of `CompositeComponent`
# which satisfies some standard interface, rather than it needing to be `implement`ed?
"""
    ComponentGroup <: GenericComponent
    ComponentGroup(subcomponents)

A group of several components, with names given by indices if `subcomponents` is a `Tuple`
and keys if `subcomponents` is a `NamedTuple`.
"""
struct ComponentGroup{T} <: GenericComponent
    subcomponents::T
end
inputs(c::ComponentGroup) = CompositeValue(map(inputs, c.subcomponents))
outputs(c::ComponentGroup) = CompositeValue(map(outputs, c.subcomponents))
implement(c::ComponentGroup, ::Target) =
    CompositeComponent(inputs(c), outputs(c), c.subcomponents, Iterators.flatten((
        (
            Input(i => inname) => CompIn(i, inname)
            for (i, sc) in pairs(c.subcomponents) for inname in keys_deep(inputs(sc))
        ),
        (
            CompOut(i, outname) => Output(i => outname)
            for (i, sc) in pairs(c.subcomponents) for outname in keys_deep(outputs(sc))
        )
    )), c)

"""
    IndexedComponentGroup(subcomponents)

A `ComponentGroup` with subcomponents named via indices of values in `subcomponents`.
"""
IndexedComponentGroup(t) = ComponentGroup(Tuple(t))

"""
    NamedComponentGroup(subcomponents)

A `ComponentGroup` with subcomponent named via keys given keys, given a `subcomponents`
iterator over `(key::Symbol, subcomponent)` pairs.
"""
NamedComponentGroup(n) = ComponentGroup(NamedTuple(n))