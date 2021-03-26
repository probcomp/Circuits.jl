# TODO: docstrings; remove old code

##############
# Core Types #
##############

"""
    abstract type Value end

A "wire" in a circuit diagram, which can transmit a value of a certain type.
"""
abstract type Value end

"""
    abstract type GenericValue <: Value end

A `Value` which is not a `PrimitiveValue` nor `CompositeValue`.
"""
abstract type GenericValue <: Value end

"""
    abstract type PrimitiveValue{T} <: Value end

A primitive value for `T  <: Target`.  Ie. the simulator for `T`
can directly use this type of value.
"""
abstract type PrimitiveValue{Target} <: Value end

"""
    implement(::Value, ::Target)

Make progress implementing the value for the target, so that a finite
number of repeated calls to `implement` will yield a value `v` so that `is_implementation_for(v, t)` is true.
"""
implement(v::Value, t::Target) = no_impl_error(v, t)
implement(v::PrimitiveValue{T1}, t::T2) where {T1 <: Target, T2 <: Target} =
    if T1 <: T2
        v
    else
        error("Cannot implement $v, a PrimitiveValue{$T1}, for target $t.")
    end

"""
    is_implementation_for(::Value, ::Target)

Whether the given value is an implementation for the given target (ie. whether it is supported
by the simulator for that target).
"""
is_implementation_for(::PrimitiveValue{<:T}, ::T) where {T <: Target} = true
is_implementation_for(::PrimitiveValue, ::Target) = false
is_implementation_for(::GenericValue, ::Target) = false

"""
    implement_deep(::Value, t::Target)

Implement the value for the target recursively, yielding a value `v` such that
`is_implementation_for(v, t)` is true.
"""
implement_deep(v::PrimitiveValue, t::Target) = implement(v, t)
implement_deep(v::GenericValue, t::Target) = implement_deep(implement(v, t), t)

"""
    abstract(v::Value)

Returns the value which was `implement`ed to produce `v` if it exists and is available;
otherwise returns `nothing`.
"""
abstract(::Value) = nothing

"""
    target(v::Value)

If `v` is a _concrete value_ which has 1 implementation for 1 target,
return the `Target` `v` can be implemented for.  Else, returns `nothing`.
"""
target(::Value) = nothing

### Composite value ###

include("composite_value.jl")

### Binary ###
"""
    Binary <: GenericValue

A value which at any given time either represents a `1` or `0`.
"""
struct Binary <: GenericValue end

"""
    compiles_to_binary(v::Value, t::Target)

Whether this value can be compiled for this target
into a `CompositeValue` made entirely of `Binary` values
(or values which are implementations of `Binary`).
"""
compiles_to_binary(v::Value, t::Target) = _compiles_to_binary(v, t)
_compiles_to_binary(v::CompositeValue, t::Target) = all(compiles_to_binary(val, t) for val in v.vals)
_compiles_to_binary(v::Value, t::Target) = compiles_to_binary(implement(v, t), t)
_compiles_to_binary(::Binary, ::Target) = true