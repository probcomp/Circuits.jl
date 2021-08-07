function add_node!(g, name::Symbol, 
        subcomp::InputOutputComponent)
    node_attrs = Dict{Symbol, Any}(:shape => "circle",
                                   :label => "$(name)")
    CircuitViz.add_vertex!(g, node_attrs)
end

function add_node!(g, name::Symbol, 
        subcomp::PrimitiveComponent)
    node_attrs = Dict{Symbol, Any}(:shape => "triangle",
                                   :label => "$(name)")
    CircuitViz.add_vertex!(g, node_attrs)
end

function add_node!(g, name::Symbol, 
        subcomp::CompositeComponent)
    node_attrs = Dict{Symbol, Any}(:shape => "square",
                                   :label => "$(name)")
    CircuitViz.add_vertex!(g, node_attrs)
end

function add_node!(g, name::String, 
        subcomp::InputOutputComponent)
    node_attrs = Dict{Symbol, Any}(:shape => "circle",
                                   :label => name)
    CircuitViz.add_vertex!(g, node_attrs)
end

function add_node!(g, name::String, 
        subcomp::PrimitiveComponent)
    node_attrs = Dict{Symbol, Any}(:shape => "triangle",
                                   :label => name)
    CircuitViz.add_vertex!(g, node_attrs)
end

function add_node!(g, name::String, 
        subcomp::CompositeComponent; 
        node_attrs = Dict{Symbol, Any}(:shape => "square",
                                       :label => name))
    CircuitViz.add_vertex!(g, node_attrs)
end

function add_node!(g, name_fn::Function, subcomp::CompositeComponent)
    node_attrs = Dict{Symbol, Any}(:shape => "square",
                                   :label => repr(name_fn(subcomp)))
    CircuitViz.add_vertex!(g, node_attrs)
end

function convert_to_catlab_graph(c::CompositeComponent)
    g, idx_to_name, name_to_idx = _subcomponent_graph(c)
    new = CircuitViz.Graph()
    vs = vertices(g)
    es = edges(g)
    for v in vs
        name = getindex(idx_to_name, v)
        subcomp = getindex(c, name)
        add_node!(new, Symbol(name), subcomp) # This handles properties using dispatch.
    end
    for e in es
        src = e.src
        dst = e.dst
        CircuitViz.add_edge!(new, src, dst)
    end
    return new
end

_convert(s::Symbol) = String(s)
_convert(s::Int) = repr(s)
_convert(s::String) = s
name_converter(t) = foldr((x, y) -> _convert(x) * "\n=> " * _convert(y), t)

function convert_to_catlab_graph(c::CompositeComponent,
        name_map::Dict)
    g, idx_to_name, name_to_idx = _subcomponent_graph(c)
    new = CircuitViz.Graph()
    vs = vertices(g)
    es = edges(g)
    for v in vs
        name = getindex(idx_to_name, v)
        subcomp = getindex(c, name)
        name = getindex(name_map, name)
        name = name_converter(name)
        add_node!(new, name, subcomp) # This handles properties using dispatch.
    end
    for e in es
        src = e.src
        dst = e.dst
        CircuitViz.add_edge!(new, src, dst)
    end
    return new
end

function add_node!(g, input::Input)
    node_attrs = Dict{Symbol, Any}(:shape => "point",
                                   #:label => "$(input.id)"
                                  )
    CircuitViz.add_vertex!(g, node_attrs)
end

function add_node!(g, output::Output)
    node_attrs = Dict{Symbol, Any}(:shape => "point",
                                   #:label => "$(output.id)"
                                  )
    CircuitViz.add_vertex!(g, node_attrs)
end

function _create_name_map(state::InlineState)
    d = Dict{Int, Any}()
    rev = Dict(v => k for (k, v) in state.internals)
    for v in values(state.internals)
        if !haskey(d, v)
            d[v] = rev[v]
        elseif length(rev[v]) > length(d[v])
            d[v] = rev[v]
        end
    end
    return Dict(k => v[1 : end - 1] for (k, v) in d)
end

function convert_to_catlab_graph(state::InlineState)
    g = CircuitViz.Graph()
    primitives = Dict{Any, Any}()
    name_map = _create_name_map(state)
    count = 1
    for inp in state.inputs
        snd = inp[2]
        add_node!(g, snd)
        primitives[(snd.id, )] = count
        count += 1
    end

    for out in state.outputs
        snd = out[2]
        add_node!(g, snd)
        primitives[(snd.id, )] = count
        count += 1
    end

    edges = map(state.completed_edges) do p
        Pair(map((p.current, p.start)) do s
                 if length(s) == 1
                     if haskey(primitives, s)
                         tg = getindex(primitives, s)
                     end
                 else
                     if haskey(primitives, s[1 : end - 1])
                         tg = getindex(primitives, s[1 : end - 1])
                     else
                         tg = getindex(state.internals, s)
                         name = getindex(name_map, tg)
                         name = name_converter(name)
                         tg = getindex(state.primitive_nodes, tg)
                         add_node!(g, name, tg)
                         primitives[s[1 : end - 1]] = count
                         tg = count
                         count += 1
                     end
                 end
                 return tg
             end...)
    end
    for (k, v) in edges
        CircuitViz.add_edge!(g, k, v)
    end
    return g
end

function convert_to_catlab_graph(state::InlineState, cc_name_fn)
    g = CircuitViz.Graph()
    primitives = Dict{Any, Any}()
    name_map = _create_name_map(state)
    count = 1
    for inp in state.inputs
        snd = inp[2]
        add_node!(g, snd)
        primitives[(snd.id, )] = count
        count += 1
    end

    for out in state.outputs
        snd = out[2]
        add_node!(g, snd)
        primitives[(snd.id, )] = count
        count += 1
    end

    edges = map(state.completed_edges) do p
        Pair(map((p.current, p.start)) do s
                 if length(s) == 1
                     if haskey(primitives, s)
                         tg = getindex(primitives, s)
                     end
                 else
                     if haskey(primitives, s[1 : end - 1])
                         tg = getindex(primitives, s[1 : end - 1])
                     else
                         tg = getindex(state.internals, s)
                         name = getindex(name_map, tg)
                         name = name_converter(name)
                         tg = getindex(state.primitive_nodes, tg)
                         add_node!(g, name, tg)
                         primitives[s[1 : end - 1]] = count
                         tg = count
                         count += 1
                     end
                 end
                 return tg
             end...)
    end
    for (k, v) in edges
        CircuitViz.add_edge!(g, k, v)
    end
    return g
end

function viz!(state::InlineState; 
        base_path = "$(gensym("digraph"))")
    g = convert_to_catlab_graph(state)
    CircuitViz.compile!(g; base_path = base_path)
end

function viz!(c::CompositeComponent)
    g = convert_to_catlab_graph(c)
    CircuitViz.compile!(g)
end

function viz!(c::CompositeComponent, names::Dict)
    g = convert_to_catlab_graph(c, names)
    CircuitViz.compile!(g)
end
