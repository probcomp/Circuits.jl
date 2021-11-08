function add_node!(g, name::Symbol, 
        subcomp::InputOutputComponent)
    node_attrs = Dict{Symbol, Any}(:shape => "circle",
                                   :label => "$(name)")
    CircuitViz.add_vertex!(g, node_attrs)
end

function add_node!(g, name::Symbol, 
        subcomp::T) where T <: PrimitiveComponent
    node_attrs = Dict{Symbol, Any}(:shape => "triangle",
                                   :label => "Neuron"
                                   )
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
                                   :label => "Neuron"
                                  )
    CircuitViz.add_vertex!(g, node_attrs)
end

function add_node!(g, name, subcomp)
    error("$(typeof(subcomp)) not supported for `add_node!`")
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
    node_attrs = Dict{Symbol, Any}(:shape => "circle",
                                   :label => "Spike"
                                  )
    CircuitViz.add_vertex!(g, node_attrs)
end

function add_node!(g, output::Output)
    node_attrs = Dict{Symbol, Any}(:shape => "circle",
                                   :label => "Spike",
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

function handle!(g, in::Input, state, 
        primitives, name_map)
    id = in.id
    node_id = primitives[(:input, id)]
    return node_id
end

function handle!(g, out::Output, state, 
        primitives, name_map)
    id = out.id
    node_id = primitives[(:output, id)]
    return node_id
end

function handle!(g, cout::CompOut, state, 
        primitives, name_map)
    id = cout.comp_name
    if haskey(primitives, (:internal, id))
        node_id = primitives[(:internal, id)]
        return node_id
    else
        name = getindex(name_map, id)
        name = name_converter(name)
        prim = state.primitive_nodes[id]
        add_node!(g, name, prim)
        new_id = length(primitives) + 1
        primitives[(:internal, id)] = new_id
        return new_id
    end
end

function handle!(g, cin::CompIn, state, 
        primitives, name_map)
    id = cin.comp_name
    if haskey(primitives, (:internal, id))
        node_id = primitives[(:internal, id)]
        return node_id
    else
        name = getindex(name_map, id)
        name = name_converter(name)
        prim = state.primitive_nodes[id]
        add_node!(g, name, prim)
        new_id = length(primitives) + 1
        primitives[(:internal, id)] = new_id
        return new_id
    end
end

function convert_to_catlab_graph(state::InlineState)
    g = CircuitViz.Graph()
    name_map = _create_name_map(state)
    new_edges = create_new_edges_from_state(state)
    primitives = Dict{Any, Any}()

    for inp in state.inputs
        snd = inp[2]
        add_node!(g, snd)
        primitives[(:input, snd.id)] = length(primitives) + 1
    end

    for out in state.outputs
        snd = out[2]
        add_node!(g, snd)
        primitives[(:output, snd.id)] = length(primitives) + 1
    end

    edges = map(new_edges) do (from, to)
        f = handle!(g, from, state, primitives, name_map)
        t = handle!(g, to, state, primitives, name_map)
        Pair(f, t)
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

function viz!(c::CompositeComponent;
        base_path = "$(gensym("digraph"))")
    g = convert_to_catlab_graph(c)
    CircuitViz.compile!(g; base_path = base_path)
end

function viz!(c::CompositeComponent, names::Dict)
    g = convert_to_catlab_graph(c, names)
    CircuitViz.compile!(g)
end
