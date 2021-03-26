"""
    abstract type NodeName end

The name of a node in a `CompositeComponent` graph.
Is either an `Input` to the component, an `Output` from it,
or an input to/output from a sub-component of the composite component
(a `CompIn` or `CompOut`).

In a `CompIn` or `CompOut`, sub-components in this `c::CompositeComponent` are named using their name in `c`.

In any `NodeName`, an input/output value for a component `c`
(where `c` is either a top-level component or a subcomponent)
may either directly be a value in `inputs(c)` or `outputs(c)`, or may be a value nested
within a `CompositeValue` in `inputs(c)` or `outputs(c)`.
A value in `inputs(c)` or `outputs(c)` is referred to using the name within the `inputs`/`outputs`
`CompositeValue`.
A nested value is referred to using a linked-list
`n_1 => (n_2 => ... (n_{n-1} => n_n))`.

(Eg. `Inputs(x => (y => z))` refers to `inputs(c)[x => y => z] == inputs(c)[x][y][z]`.)
"""
abstract type NodeName end

"""
    Input <: NodeName
    Input(name)

An input value to a composite component with the given `name`.
"""
struct Input{ID} <: NodeName
    id::ID
end
"""
    Output <: NodeName
    Output(name)

An output value from a composite component with the given `name`.
"""
struct Output{ID} <: NodeName
    id::ID
end

Base.:(==)(a::Input, b::Input) = (a.id == b.id)
Base.:(==)(a::Output, b::Output) = (a.id == b.id)
Base.hash(i::Input, h::UInt) = hash(i.id, h)
Base.hash(o::Output, h::UInt) = hash(o.id, h)

# TODO: better docs about what types `comp_name` and `in_name`
# can be, and about the fact that we move nesting to the values
"""
    CompIn <: NodeName
    CompIn(comp_name, in_name)

An input value to a sub-component named `comp_name` in a composite component,
with name `in_name` in the subcomponent.
"""
struct CompIn{I1, I2} <: NodeName
    comp_name::I1
    in_name::I2
    CompIn(i1::I1, i2::I2) where {
        I1 <: Union{Symbol, Integer}, I2 <: Union{Symbol, Integer, Pair}
    } = new{I1, I2}(i1, i2)
end
"""
    CompOut <: NodeName
    CompOut(comp_name, out_name)

An output value from a sub-component named `comp_name` in a composite component,
with name `out_name` in the subcomponent.
"""
struct CompOut{I1, I2} <: NodeName
    comp_name::I1
    out_name::I2
    CompOut(i1::I1, i2::I2) where {
        I1 <: Union{Symbol, Integer}, I2 <: Union{Symbol, Integer, Pair}
    } = new{I1, I2}(i1, i2)
end

"""
    CompIn(x_1 => ... => x_n, inname)

This is transformed into `CompIn(x_1, x_2 => ... => x_n => inname)`,
so accessing nested components transforms into accessing nested values
in a `ComponentGroup`.
"""
CompIn(p::Pair, inname) = CompIn(p.first, nest(p.second, inname))

"""
    CompOut(x_1 => ... => x_n, outname)

This is transformed into `CompOut(x_1, x_2 => ... => x_n => outname)`.
so accessing nested components transforms into accessing nested values
in a `ComponentGroup`.
"""
CompOut(p::Pair, outname) = CompOut(p.first, nest(p.second, outname))

nest(p, v) = p => v
nest(p::Pair, v) = p.first => nest(p.second, v)

Base.:(==)(a::CompIn, b::CompIn) = (a.comp_name == b.comp_name && a.in_name == b.in_name)
Base.:(==)(a::CompOut, b::CompOut) = (a.comp_name == b.comp_name && a.out_name == b.out_name)
Base.hash(i::CompIn, h::UInt) = hash(i.comp_name, hash(i.in_name, h))
Base.hash(o::CompOut, h::UInt) = hash(o.comp_name, hash(o.out_name, h))

Base.show(io::IO, i::Input) = print(io, "Input($(valname(i)))")
Base.show(io::IO, i::Output) = print(io, "Output($(valname(i)))")
Base.show(io::IO, i::CompIn) = print(io, "CompIn($(i.comp_name), $(valname(i)))")
Base.show(io::IO, i::CompOut) = print(io, "CompOut($(i.comp_name), $(valname(i)))")
Base.show(io::IO, ::MIME"text/plain", n::NodeName) = show(io, n)

valname(v::Union{Input, Output}) = v.id
valname(v::CompIn) = v.in_name
valname(v::CompOut) = v.out_name

# """
#     append_to_valname(n::NodeName, rest)

# Returns a new nodename which is like the first, but with `rest` nested at the end of the value's name.
# """
# append_to_valname(i::Input, rest) = Input(nest(valname(i), rest))
# append_to_valname(o::Output, rest) = Output(nest(valname(i), rest))
# append_to_valname(i::CompIn, rest) = CompIn(i.comp_name, nest(valname(i), rest))
# append_to_valname(i::CompOut, rest) = CompOut(i.comp_name, nest(valname(i), rest))
