module Circuits

using Distributions: Categorical, ncategories, probs
using Memoization

# returns a collection of the same top-level type mapping `name => name`
names(t::Tuple) = Tuple(1:length(t))
names(n::NamedTuple) = (;(k=>k for k in keys(n))...)

no_impl_error(::V, ::T) where {V, T} = error("No implementation for type `$V` defined for target `$T`.")

include("target.jl")
include("value.jl")
include("component.jl")

"""
    can_implement(c::Component, t::Target)
    can_implement(v::Value, t::Target)

Whether the given component/value can be implemented in this target.
"""
can_implement(::K, ::T) where {K <: Union{Component, Value}, T <: Target} = hasmethod(implement, Tuple{K, T})

"""
    has_abstact_of_type(value::Value, type::Type)
    has_abstact_of_type(component::Component, type::Type)

Whether some abstract version of the value/component is of type `type`.
(This checks `abstract(item)`, `abstract(abstract(item))`, and so on.)
"""
has_abstract_of_type(item::Union{Value, Component}, type::Type) =
    isa(item, type) || has_abstract_of_type(abstract(item), type)
has_abstract_of_type(::Nothing, t::Type) = t == Nothing

"""
    has_abstract(::Value, abstract)
    has_abstract(::Component, abstract)

Whether some sufficiently abstract version of the given Value/Component
is equal to `abstract` (equality checked via `==`).
"""
has_abstract(item::Union{Value, Component}, abst) =
    item == abst || has_abstract(abstract(item), abst)
has_abstract(n::Nothing, a) = n == a

"""
    abstract_to_type(value::Value, type::Type)
    abstract_to_type(component::Component, type::Type)

Abstract the value or component until reaching an abstract version of type `type`;
return nothing if no abstract version of this type exists.
"""
abstract_to_type(item::T, ::Type{T}) where {T <: Union{Value, Component}} = item
abstract_to_type(item::Union{Value, Component}, t::Type) = abstract_to_type(abstract(item), t)
abstract_to_type(::Nothing, ::Type) = nothing

export Target
export Value, PrimitiveValue, GenericValue, CompositeValue
export Component, PrimitiveComponent, GenericComponent, CompositeComponent
export abstract, target, inputs, outputs, implement, implement_deep, is_implementation_for
export keys_deep, length_deep
export IndexedValues, NamedValues
export ComponentGroup, IndexedComponentGroup, NamedComponentGroup
export WithAbstractComponent, WithAbstract, RelabeledIOComponent
export NodeName, Input, Output, CompIn, CompOut
export Binary
export can_implement, compiles_to_binary
export merge_composite_values
export inputters, receivers
export topologically_ordered_subcomponents
export has_abstract_of_type, abstract_to_type

end