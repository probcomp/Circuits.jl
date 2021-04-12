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

"""
    WithAbstractComponent <: GenericComponent
    WithAbstractComponent(component::Component, abstract::Component)
    WithAbstract(component::Component, abstract::Component)

A component implemented in the same way as `component`, but with a different `abstract` form.
(Useful for implementing one component as another.)
"""
struct WithAbstractComponent <: GenericComponent
    component::Component
    abstract::Component
end
WithAbstract(c::Component, a::Component) = WithAbstractComponent(c, a)
# TODO: I forget the minimal amount I need to implement; this may be overkill
abstract(c::WithAbstractComponent) = c.abstract
target(c::WithAbstractComponent) = target(c.component)
inputs(c::WithAbstractComponent) = inputs(c.component)
outputs(c::WithAbstractComponent) = outputs(c.component)
implement(c::WithAbstractComponent, t::Target) = implement(c.component, t)
is_implementation_for(c::WithAbstractComponent, t::Target) = is_implementation_for(c.component, t)

"""
    RelabeledIOComponent(component::Component, input_relabels, output_relabels, abstact::Union{Component, Nothing})

Wraps `component` to reroute inputs according to `input_relabels` and outputs according to `output_relabels`.
Each relabels arg should be an iterator over pairs `old_name => new_name` describing the input/output
to be relabeled.  All these names should be symbols; we do not currently support relabeling integer-indexed input or output values.
If given an `abstract`, takes `abstract` to be the abstract version of this component.
"""
struct RelabeledIOComponent
    component::Component
    input_relabels::Dict
    output_relabels::Dict
    abstract::Union{Nothing, Component}
end
RelabeledIOComponent(c, i, o, a=nothing) = RelabeledIOComponent(Dict(c...), Dict(i...), Dict(o...), a)
relabeled_key(k, relabels) = get(relabels, k, k)
function Circuits.inputs(c::RelabeledIOComponent)
    @assert inputs(c).vals isa NamedTuple || isempty(c.input_relabels) "Current implementation can only relabel symbol (not Int)-addressed values"
    return CompositeValue((;(
        relabeled_key(k, c.input_relabels) => v for (k, v) in pairs(inputs(c).vals)
    )...))
end
function outputs(c::RelabeledIOComponent)
    @assert inputs(c).vals isa NamedTuple || isempty(c.output_relabels) "Current implementation can only relabel symbol (not Int)-addressed values"
    return CompositeValue((;(
        relabeled_key(k, c.output_relabels) => v for (k, v) in pairs(inputs(c).vals)
    )...))
end
implement(c::RelabeledIOComponent) = CompositeComponent(
    inputs(c), outputs(c),
    (component=c.component),
    Iterators.flatten((
        (
            Input(relabeled_key(k, c.input_relabels)) => CompIn(:component, k)
            for k in keys(inputs(c.component))
        ),
        (
            CompOut(:component, k) => Output(relabeled_key(k, c.output_relabels))
            for k in keys(output(c.component))
        )
    )),
    c
)
abstract(c::RelabeledIOComponent) = c.abstract
target(c::RelabeledIOComponent) = target(c.component)