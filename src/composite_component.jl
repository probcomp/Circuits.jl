include("nodename.jl")

# TODO: more detailed documentation?
# TODO: better type propogation for performance?
"""
    CompositeComponent <: Component

A `Component` represented as a graph of wires connecting inputs & outputs of subcomponents.

Sub-components may be accessed via Base.getindex, so `c[name]` yields the subcomponent
with `name` in `c::CompositeComponent`.  `c[nothing]` will yield `c`, and `c[x1 => (x2 => (... => (x_n)))]`
will yield `c[x1][x2][...][x_n]`.  Thus, a subcomponent name may be `nothing`, a top-level name,
or, to refer to a nested subcomponent, a nested name implemented as a `Pair`-based linked list.
"""
struct CompositeComponent <: Component #{InID, OutID, SC} <: Component
    input::CompositeValue #{InID}
    output::CompositeValue #{OutID}
    subcomponents #::SC # `Tuple` or `NamedTuple` of subcomponents
    node_to_idx::Dict{NodeName, UInt} # maps from `NodeName` to index of this node in the graph
    idx_to_node::Vector{NodeName} # maps index of a node in the graph to its `NodeName`
    graph::SimpleDiGraph # graph of connections between input/output values
    abstract::Union{Nothing, Component} # the component which was `implement`ed to obtain this `CompositeComponent`
    
    # ensure `subcomponents` is either a Tuple or NamedTuple of Components
    # CompositeComponent(i::CompositeValue{I}, o::CompositeValue{O}, sc::T, ni, in, g, abs=nothing) where {I, O, T <: tup_or_namedtup(Component)} =
    #     new{I, O, basetype(T)}(i, o, sc, ni, in, g, abs)

    # CompositeComponent(i::CompositeValue{I}, o::CompositeValue{O}, sc::T, args...) where {I, O, T <: Tuple{Vararg{<:Component}}} =
    #     new{I, O, Tuple}(i, o, sc, args...)
    # CompositeComponent(i::CompositeValue{I}, o::CompositeValue{O}, sc::NT, args...) where {I, O, NT <: NamedTuple{<:Any, <:Tuple{Vararg{<:Component}}}} =
    #     new{I, O, NamedTuple}(i, o, sc, args...)
end
CompositeComponent(i, o, sc, ni, in, g) = new(i, o, sc, ni, in, g, nothing)

function _digraph_from_iter(itr, idx_to_node; identifier_info="")
    graph = try
        SimpleDiGraphFromIterator(itr)
    catch e
        if e isa KeyError && e.key isa NodeName
            @error("$(e.key) used in `edges` but does not match the nodenames derived from `input`, `output`, and `subcomponents`", exception = (e, catch_backtrace()))
        else
            @error("An error occurred while constructing a CompositeComponent (possibly due to expanding the edge iterator!).  ($identifier_info)", exception = (e, catch_backtrace()))
        end
        throw(e)
    end
    add_vertices!(graph, length(idx_to_node) - nv(graph))
    @assert nv(graph) == length(idx_to_node)
    return graph
end

function expand_edge(src::NodeName, dst::NodeName, input, output, subcomponents, node_to_idx)
    if src in keys(node_to_idx)
        # no expansion necessary
        @assert dst in keys(node_to_idx) "$dst not a known output nodename"
        return (src => dst,)
    else
        src_ext = name_extensions(src, input, output, subcomponents)
        dst_ext = name_extensions(dst, input, output, subcomponents)
        @assert Set(src_ext) == Set(dst_ext)
        return (
            append_to_valname(src, ext) => append_to_valname(dst, ext)
            for ext in src_ext
        )
    end
end

"""
    name_extensions(n::NodeName, input, output, subcomponents)

Given a nodename `n` where `v = valname(n)`, this returns an iterator over all the extensions to `v`
so that the resulting value-name is a deep index into a node in `c`.

(In other words, finds all remainders `r` such that `append_to_valname(v, r)` is a deep nodename in `c`.)
"""
name_extensions(n::NodeName, input, output, subcomponents) =
    try
       __name_extensions(n, input, output, subcomponents)
    catch e
        @error("Error occurred while looking for name extensions for $n.", exception=(e, catch_backtrace()))
        throw(e)
    end
__name_extensions(v::Input, input, _, _) = _name_extensions(input, v)
__name_extensions(v::Output, _, output, _) = _name_extensions(output, v)
__name_extensions(v::CompIn, _, _, subcomponents) = _name_extensions(inputs(subcomponents[v.comp_name]), v)
__name_extensions(v::CompOut, _, _, subcomponents) = _name_extensions(outputs(subcomponents[v.comp_name]), v)
_name_extensions(val, v) = keys_deep(val[valname(v)])

extended_idx_edge_itr(input, output, subcomponents, edges, node_to_idx) = idx_edge_itr(
    Iterators.flatten(
        Iterators.map(
            edge -> expand_edge(edge..., input, output, subcomponents, node_to_idx),
            edges
        )
    ),
    node_to_idx
)
idx_edge_itr(edges, node_to_idx) = (
    Edge(node_to_idx[src_name], node_to_idx[dst_name])
    for (src_name, dst_name) in edges
)

"""
    CompositeComponent(
        input::CompositeValue, output::CompositeValue,
        subcomponents::Union{Tuple, NamedTuple}, edges, abstract=nothing
    )

A composite component with the given inputs, outputs, subcomponents, graph edges, and abstract version.
If `subcomponents` is a `Tuple`, the subcomponent names will be `1, ..., length(subcomponents)`.
If `subcomponents` is a `NamedTuple`, the subcomponent names will be the keys in the named tuple.
`edges` should be an iterator over `Pair{<:NodeName, <:NodeName}`s of the form `src_nodename => dst_nodename`,
giving the inputs/outputs of the component and its subcomponents to connect with an edge.  (Each `src_nodename`
should be either an `Input` or `CompOut`, and each `dst_nodename` should be a `Output` or `CompIn`.)
"""
function CompositeComponent(
    input::CompositeValue,
    output::CompositeValue,
    subcomponents,
    edges, abstract::Union{Nothing, Component}=nothing
)
    idx_to_node = collect(Iterators.flatten((
        (Input(k) for k in keys_deep(input)), (Output(k) for k in keys_deep(output)),
        (CompIn(compname, inname) for (compname, subcomp) in pairs(subcomponents) for inname in keys_deep(inputs(subcomp))),
        (CompOut(compname, outname) for (compname, subcomp) in pairs(subcomponents) for outname in keys_deep(outputs(subcomp)))
    )))
    node_to_idx = Dict{NodeName, UInt}(name => idx for (idx, name) in enumerate(idx_to_node))

    graph = _digraph_from_iter(
        extended_idx_edge_itr(input, output, subcomponents, edges, node_to_idx),
        idx_to_node;
        identifier_info="CompositeComponent had abstract type = $(typeof(abstract))."
    )

    CompositeComponent(input, output, subcomponents, node_to_idx, idx_to_node, graph, abstract)
end

"""
    CompositeComponent(
        c::CompositeComponent;
        input=c.input,
        output=c.output,
        subcomponents=c.subcomponents,
        subcomponent_map::Union{Function, Nothing}=nothing,
        abstract=c.abstract
    )

Duplicate of `c::CompositeComponent`, but with changed input and/or output and/or subcomponents
and/or abstract version.

Kwarg `subcomponents` can be used to replace the subcomponents.
If a function `subcomponent_map` is given, it will be applied to each element of the given
or previous `subcomponents` to yield subcomponents for the new component.
"""
CompositeComponent(
    c::CompositeComponent;
    input=c.input,
    output=c.output,
    subcomponents=c.subcomponents,
    subcomponent_map::Union{Function, Nothing}=nothing,
    abstract=c.abstract
) = CompositeComponent(
    input, output,
    subcomponent_map === nothing ? subcomponents : map(subcomponent_map, subcomponents),
    c.node_to_idx, c.idx_to_node, c.graph, abstract
)

inputs(c::CompositeComponent) = c.input
outputs(c::CompositeComponent) = c.output
abstract(c::CompositeComponent) = c.abstract

Base.getindex(c::CompositeComponent, ::Nothing) = c
Base.getindex(c::CompositeComponent, p::Pair) = c[p.first][p.second]
Base.getindex(c::CompositeComponent, name) = c.subcomponents[name]

Base.:(==)(a::CompositeComponent, b::CompositeComponent) = (
       inputs(a) == inputs(b)
    && outputs(a) == outputs(b)
    && a.subcomponents == b.subcomponents
    && all(has_edge(a, edge) for edge in get_edges(b))
    && all(has_edge(b, edge) for edge in get_edges(a))
    # Equality does not rely on having the same abstract component!
    # && abstract(a) == abstract(b)
)
Base.hash(a::CompositeComponent, h::UInt) = hash(inputs(a), hash(outputs(a), hash(subcomponents(a), hash(collect(get_edges(a)), h))))

"""
    Base.map(f, c::CompositeComponent)

A CompositeComponent with `f` applied to each subcomponent of `c`.
All connections & values are the same.
"""
Base.map(f, c::CompositeComponent) =
    CompositeComponent(c.input, c.output, map(f, c.subcomponents), c.node_to_idx, c.idx_to_node, c.graph, c.abstract)

"""
    get_edges(c::CompositeComponent)

Iterator over edges in the composite component graph.
"""
get_edges(c::CompositeComponent) = (
        c.idx_to_node[src(edge)] => c.idx_to_node[dst(edge)]
        for edge in edges(c.graph)
    )

has_edge(c::CompositeComponent, (src, dst)) =
    Edge(c.node_to_idx[src], c.node_to_idx[dst]) in edges(c.graph)

is_implementation_for(c::CompositeComponent, t::Target) =
    is_implementation_for(inputs(c), t) && is_implementation_for(outputs(c), t) && all(
        is_implementation_for(subc, t) for subc in values(c.subcomponents)
    )

"""
    implement(c::CompositeComponent, t::Target,
        subcomponent_filter::Function = (n -> true);
        input_filter::Function = (n -> true),
        output_filter::Function = (n -> true)
        deep=false
    )

Make progress implementing `c` for `t`.  Implement each subcomponent
whose name passes `subcomponent_filter`.  Each input/output will be implemented
as deeply as is needed for it to match any `CompIn`s or `CompOut`s it is connected to.
    If no inputs or outputs are connected to subcomponents, 
each input value whose name
passes `input_filter`, and each output whose name passes `output_filter` will be implemented.

If `deep` is false, `implement` will be used for recursive calls (shallow one-step implement);
if `deep` is true, `implement_deep` will be used (fully implementing `c` for `t`).
"""
function implement(c::CompositeComponent, t::Target,
    subcomponent_filter::Function = (n -> true);
    input_filter::Function = (n -> true),
    output_filter::Function = (n -> true),
    deep=false
)
    # TODO: be smarter about which subcomponents we implement, so that we don't
    # end up connecting values implemented to different levels
    new_comps = map(names(c.subcomponents), c.subcomponents) do name, subcomp
        if subcomponent_filter(name)
            if deep
                implement_deep(subcomp, t)
            else
                implement(subcomp, t)
            end
        else
            subcomp
        end
    end

    # now we want to implement the top-level inputs/outputs so that they are compatible
    # with the `CompIn` and `CompOut` from the subcomponents
    new_in, new_out = implement_inputs_and_outputs(c, new_comps, input_filter, output_filter, t, deep)

    new_idx_to_node = collect(Iterators.flatten((
        (Input(k) for k in keys_deep(new_in)), (Output(k) for k in keys_deep(new_out)),
        (CompIn(compname, inname) for (compname, subcomp) in pairs(new_comps) for inname in keys_deep(inputs(subcomp))),
        (CompOut(compname, outname) for (compname, subcomp) in pairs(new_comps) for outname in keys_deep(outputs(subcomp)))
    )))
    new_node_to_idx = Dict{NodeName, UInt}(name => idx for (idx, name) in enumerate(new_idx_to_node))

    new_graph = _digraph_from_iter(
        idx_edge_itr( # TODO: do we want `extended_idx_edge_itr` instead?
            Iterators.flatten(
                expand_edges(edge, new_in, new_out, new_comps, c)
                for edge in get_edges(c)
            ),
            new_node_to_idx
        ),
        new_idx_to_node;
        identifier_info="Occurred during implementation of component with abstract type $(typeof(abstract(c)))."
    )

    return CompositeComponent(new_in, new_out, new_comps, new_node_to_idx, new_idx_to_node, new_graph, c)
end
# implement the inputs and outputs for a CompositeComponent deeply enough that they match
# the `CompIn`s and `CompOut`s which they are connected to
function implement_inputs_and_outputs(c, new_comps, input_filter, output_filter, target, deep)
    # first, we figure out which `CompOut`s lead to outputs, and which `CompIn`s lead to inputs
    input_connections = Dict()
    output_connections = Dict()
    for (src, dst) in get_edges(c)
        if src isa Input && dst isa CompIn
            input_connections[src] = push!(get(input_connections, src, []), dst)
        end
        if dst isa Output && src isa CompOut
            output_connections[dst] = push!(get(input_connections, dst, []), src)
        end
    end
    
    # implement the values to the correct thing
    # (we implement to this rather than just using the CompIn/CompOut value so that we can error check
    # in case this implementation is impossible)
    inval(compin) =
        try
            inputs(new_comps[compin.comp_name])[valname(compin)]
        catch e
            @error("Error while looking up input `$(valname(compin))` in new component $(compin.comp_name).", exception=(e, catch_backtrace()))
            error()
        end

    outval(compout) = 
        try
            outputs(new_comps[compout.comp_name])[valname(compout)]
        catch e
            @error("Error while looking up output `$(valname(compout))` in new component $(compout.comp_name).", exception=(e, catch_backtrace()))
            error()
        end

    get_new_input(key) =
        if haskey(input_connections, Input(key))
            val = inval(first(input_connections[Input(key)]))
            @assert all(inval(compin) == val for compin in input_connections[Input(key)]) "Implementing this CompositeComponent leads to `CompIn`s which are connected to the same output being implemented at different levels!"
            # TODO: there are situations where this check doesn't work very well, if we have deep-implemented
            # a subcomponent where the intermediary transformations of the values are non-standard.
            if !(has_abstract(val, inputs(c)[key]) || can_implement_to(inputs(c)[key], val, target))
                @warn """
                    While implementing a CompositeComponent [with abstract type $(typeof(abstract(c)))],
                    we need to connect value $val at key $key to $(inputs(c)[key]), but the latter is not an abstract
                    version of the first, nor can the first be implemented into the second!
                """
            end
            val
        else
            val = inputs(c)[key] 
            if !(val isa PrimitiveValue) && input_filter(key)
                (deep ? implement_deep : implement)(val, target)
            else
                val
            end
        end
    get_new_output(key) =
        if haskey(output_connections, Output(key))
            val = outval(first(output_connections[Output(key)]))
            @assert all(outval(compout) == val for compout in output_connections[Output(key)]) "Implementing this CompositeComponent leads to `CompOut`s which are connected to the same output being implemented at different levels!"
            if !(has_abstract(val, outputs(c)[key]) || can_implement_to(outputs(c)[key], val, target))
                @warn """
                While implementing a CompositeComponent [with abstract type $(typeof(abstract(c)))],
                we need to connect value $val at key $key to $(outputs(c)[key]), but the latter is not an abstract
                version of the first, nor can the first be implemented into the second for target $(target)!
                """
            end
            val
        else
            val = outputs(c)[key]
            if !(val isa PrimitiveValue) && output_filter(key)
                (deep ? implement_deep : implement)(val, target)
            else
                val
            end
        end

    # construct the new inputs/outputs
    return (
        NestedCompositeValue(key => get_new_input(key) for key in keys_deep(inputs(c))),
        NestedCompositeValue(key => get_new_output(key) for key in keys_deep(outputs(c)))
    )
end

can_implement_to(val, goal, target) =
    try
        implemented = implement(val, target)
        implemented == goal || can_implement_to(implemented, goal, target)
    catch
        return false
    end

function expand_edges((src, dst), new_in, new_out, new_comps, c)
    src_wrapper, src_val = get_val(src, new_in, new_out, new_comps)
    dst_wrapper, dst_val = get_val(dst, new_in, new_out, new_comps)
    @assert (src_val == dst_val) "While implementing a CompositeComponent with abstract type $(typeof(abstract(c))), implementing values on edge $src → $dst led to mismatch: $src_val → $dst_val."
    
    if src_val isa CompositeValue
        return (
            src_wrapper(key) => dst_wrapper(key)
            for key in keys_deep(src_val)
        )
    else
        return (src => dst,)
    end
end
get_val(name::Input, new_in, _, _) = (remainder -> Input(nest(valname(name), remainder)), new_in[valname(name)])
get_val(name::Output, _, new_out, _) = (remainder -> Output(nest(valname(name), remainder)), new_out[valname(name)])
get_val(name::CompIn, _, _, new_comps) = (remainder -> CompIn(name.comp_name, nest(valname(name), remainder)), inputs(new_comps[name.comp_name])[valname(name)])
get_val(name::CompOut, _, _, new_comps) = (remainder -> CompOut(name.comp_name, nest(valname(name), remainder)), outputs(new_comps[name.comp_name])[valname(name)])

"""
    implement(c::CompositeComponent, t::Target, subcomp_names...)
    implement_deep(c, t::Target, subcomp_names...)

Implement or deep-implement the subcomponents of `c` with names in `subcomp_names`.
This will not implement the input/output values at all; if that is needed, use
the more general `implement(::CompositeComponent, ...)` with name filtering.
"""
implement(c::CompositeComponent, t::Target, subcomp_names...; kwargs...) =
    implement(c, t, in(subcomp_names); kwargs...)

implement_deep(c::CompositeComponent, t::Target, subcomp_names...; kwargs...) =
    implement(c, t, subcomp_names...; deep=true)

"""
    does_output(c::CompositeComponent, name::Union{Input, CompOut})

True if the node with the given name is connected to an `Output`; false false otherwise.
"""
# TODO: should we try to be smart about whether we iterate through the neighbors or the outputs?
does_output(c::CompositeComponent, name::Union{Input, CompOut}) =
begin
    @assert c.node_to_idx[name] <= nv(c.graph)
    any(
        c.idx_to_node[idx] isa Output
        for idx in neighbors(c.graph, c.node_to_idx[name])
    )
end

"""
    receivers(c::CompositeComponent: name::NodeName)

Iterator over all the `NodeName`s which receive output from the node named `name`.

(If the component is valid, each element of the outputted iterator
will either be an `Input`, `Output` or `CompIn`.)
"""
function receivers(c::CompositeComponent, name::CompIn)
    r = _receivers(c, name)
    @assert isempty(r)
    r
end
receivers(c::CompositeComponent, name::Union{Input, Output, CompOut}) = _receivers(c, name)

_receivers(c::CompositeComponent, name::NodeName) = (
        c.idx_to_node[idx] for idx in outneighbors(c.graph, c.node_to_idx[name])
    )

"""
    inputters(c::CompositeComponent: name::NodeName)

Iterator over all the `NodeName`s which send an input to the node named `name`.

(If the component is valid, each element of the outputted iterator
will either be an `Input`, `Output` or `CompOut`.)
"""

inputters(c::CompositeComponent, name::Union{Input, Output, CompIn}) = _inputters(c, name)
function inputters(c::CompositeComponent, name::CompOut)
    i = _inputters(c, name)
    @assert isempty(i)
    i
end
_inputters(c::CompositeComponent, name::NodeName) = (
    c.idx_to_node[idx] for idx in inneighbors(c.graph, c.node_to_idx[name])
)

"""
    topologically_ordered_subcomponents(c::CompositeComponent)

If `c` is acyclic, an iterator over (name, subcomponent) pairs, in topological order.
If `c` is cyclic, returns `nothing`.
"""
topologically_ordered_subcomponents(c::CompositeComponent) = 
    let (nodegraph, idx_to_name, name_to_idx) = _subcomponent_graph(c)
        if is_cyclic(nodegraph)
            nothing
        else
            (
                let name = idx_to_name[idx]
                    (name, c.subcomponents[name])
                end
                for idx in LightGraphs.topological_sort_by_dfs(nodegraph)
            )
        end
    end

"""
Return a graph on the subcomponents of the composite component (rather
than on the NodeNames).
"""
# This is currently not an externally-facing function, but we could make it one.
function _subcomponent_graph(c::CompositeComponent)
    idx_to_name = collect(keys(c.subcomponents))
    name_to_idx = Dict(n => i for (i, n) in enumerate(idx_to_name))
    graph = SimpleDiGraph(length(idx_to_name))
    for (inname, outname) in get_edges(c)
        if inname isa CompOut && outname isa CompIn
            add_edge!(graph, Edge(name_to_idx[inname.comp_name], name_to_idx[outname.comp_name]))
        end
    end
    return (graph, idx_to_name, name_to_idx)
end