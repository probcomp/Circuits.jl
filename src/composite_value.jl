"""
    CompositeValue <: Value
    CompositeValue(vals::Union{Tuple, NamedTuple}, abstract=nothing)

A value composed from other values.

If constructed with `vals::Tuple`, the sub-values are named `1, ..., length(vals)`;
if constructed with `vals::NamedTuple`, the sub-values are named using the named tuple keys.
`abstract` is the more abstract value which was implemented to yield this composite value.

Sub-values may be accessed using `Base.getindex`, so `v[name]` yields the subvalue with that name
in `v::CompositeValue`.  To access a sub-value nested within several layers of `CompositeValue`s,
one may use `v[x1 => (x2 => ... => (x_n))]`, which equals `v[x1][x2][...][x_n]`.

`Base.pairs(::CompositeComponent)` iterates over `(val_name, sub_value)` pairs,
`Base.keys` gives an iterator over the names, and `Base.values` gives an iterator over the sub-values.
"""
struct CompositeValue <: Value# {T} <: Value
    vals #::T
    abstract::Union{Value, Nothing}
    function CompositeValue(v, abst)
        @assert v isa Tuple || v isa NamedTuple "CompositeValue should wrap a Tuple or NamedTuple of values"
        @assert all(x isa Value for x in v) "When constructing a CompositeValue, not all given sub-values were actually values!  The following non-values were given: $([x for x in v if !isa(x, Value)])"
        return new(v, abst)
    end
    # CompositeValue(v::T, abst=nothing) where {T <: tup_or_namedtup(Value)} = new{basetype(T)}(v, abst)
    # CompositeValue(v::T, args...) where {T <: Tuple{Vararg{<:Value}}} = new{Tuple}(vals, args...)
    # CompositeValue(v::T, args...) where {T <: NamedTuple{<:Any, <:Tuple{Vararg{<:Value}}}} = new{NamedTuple}(vals, args...)
end
CompositeValue(vals) = CompositeValue(vals, nothing)

"""
    IndexedValues(t)

Given iterator `t` over values,
a `CompositeValue` with sub-value names, `1, ..., length(t)` and values
given by iterating through `t`.
"""
IndexedValues(t, abstract=nothing) = CompositeValue(Tuple(t), abstract)

"""
    NamedValues(t...)

Given a vararg `t` of `(name::Symbol, value::Value)` pairs,
a `CompositeValue` with the given values at the given names.
"""
NamedValues(t...) = CompositeValue((;t...))

Base.pairs(v::CompositeValue) = Base.pairs(v.vals)
Base.keys(v::CompositeValue) = Base.keys(v.vals)
Base.values(v::CompositeValue) = Base.values(v.vals)
Base.length(v::CompositeValue) = Base.length(v.vals)
Base.:(==)(a::CompositeValue, b::CompositeValue) = a.vals == b.vals
Base.hash(a::CompositeValue, h::UInt) = hash(a.vals, h)

"""
    Base.map(f, v::CompositeValue)

A CompositeValue with `f` applied to each subvalue of `v`.
"""
Base.map(f, v::CompositeValue) =
    CompositeValue(map(f, v.vals), v.abstract)

# TODO: performance
length_deep(v::CompositeValue) = reduce(+, length_deep(sv) for sv in values(v); init=0)
length_deep(::PrimitiveValue) = 1
length_deep(::GenericValue) = 1

"""
    keys_deep(v::CompositeValue)

An iterator over all the nested value names (nesting until reaching
a non-composite value).

### Example
```julia
c = CompositeValue((
    a = CompositeValue((SpikeWire(), SpikeWire())).
    b = CompositeValue((k=SomeGenericValue(),)),
    c = SomeGenericValue()
))
collect(keys_deep)(c) # == [:a => 1, :a => 2, :b => :k, :c]
```
"""
keys_deep(v::CompositeValue) = Iterators.flatten((
    val isa CompositeValue ? (k => subkey for subkey in keys_deep(val)) : (k,)
    for (k, val) in Base.pairs(v)
))

abstract(v::CompositeValue) = v.abstract

Base.getindex(cv::CompositeValue, k) = cv.vals[k]
Base.getindex(cv::CompositeValue, p::Pair) = cv.vals[p.first][p.second]

is_implementation_for(v::CompositeValue, t::Target) = all(is_implementation_for(val, t) for val in values(v))

"""
    implement(v::CompositeValue, t::Target, filter::Function = (x -> true); deep=false)

Make progress implementing `v` for `t`.  All subvalues whose name `n` is such that `filter(n) == true`
will be implemented; by default all subvalues are implemented.

If `deep` is false, each subvalue will be implemented one "level" using `implement`; if `deep` is true,
each subvalue will be fully implemented via `implement_deep`.
"""
implement(v::CompositeValue, t::Target, filter::Function = (x -> true); deep=false) =
    CompositeValue(
        map(names(v.vals), v.vals) do name, val
            if filter(name)
                if deep
                    implement_deep(val, t)
                else
                    implement(val, t)
                end
            else
                val
            end
        end,
        v
    )

"""
    implement(v::CompositeValue, t::Target, names...; deep=false)
    implement_deep(v::CompositeValue, t::Target, names...)

Implement or implement deep all subvalues of `v` whose name is in `names`.
"""
implement(v::CompositeValue, t::Target, names...; kwargs...) = implement(v, t, in(names); kwargs...)
implement_deep(v::CompositeValue, t::Target, names...) = implement(v, t, names...; deep=true)

"""
    NestedCompositeValue(itr)

Given an iterator over `(key, value)` pairs, where each `key` is a nested value name
(ie. an `Int`, `Symbol`, or a nested `Pair` of these), constructs a nested `CompositeValue`
with the given values at the given nested keys.
"""
function NestedCompositeValue(itr)
    top = Dict()
    for (key, val) in itr
        if key isa Pair
            top[key.first] = push!(get(top, key.first, []), (key.second, val))
        else
            top[key] = val
        end
    end
    subvals = Dict(key => val isa Vector ? NestedCompositeValue(val) : val for (key, val) in top)
    if all(k isa Int for k in keys(subvals))
        @assert length(subvals) == maximum(keys(subvals); init=0)
        return IndexedValues([subvals[i] for i=1:length(subvals)])
    elseif all(k isa Symbol for k in keys(subvals))
        return NamedValues(subvals...)
    else
        error("unexpected key: $k")
    end
end

function merge_composite_values(a::CompositeValue, b::CompositeValue)
    if a.vals isa NamedTuple
        @assert b.vals isa NamedTuple "Cannot merge int-indexed and symbol-indexed composite values"
        return CompositeValue((;Iterators.flatten((
            (
                key => merge_composite_values(a[key], b[key])
                for key in intersect(keys(a.vals), keys(b.vals))
            ),
            ( key => a[key] for key in setdiff(keys(a.vals), keys(b.vals)) ),
            ( key => b[key] for key in setdiff(keys(b.vals), keys(a.vals)) )
        ))...))
    else
        @assert a.vals isa Tuple && b.vals isa Tuple
        minlen = min(length(a.vals), min(length(b.vals)))
        longer = length(a.vals) > length(b.vals) ? a : b
        return CompositeValue(Tuple(
            if i <= minlen
                merge_composite_values(a[i], b[i])
            else
                longer[i]
            end
            for i=1:length(longer)
        ))
    end
end