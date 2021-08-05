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
    node_attrs = Dict{Symbol, Any}(:shape => "box",
                      :label => "$(name)")
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

function convert_to_catlab_graph(c::CompositeComponent,
        name_map::Dict)
    g, idx_to_name, name_to_idx = _subcomponent_graph(c)
    new = CircuitViz.Graph()
    vs = vertices(g)
    es = edges(g)
    for v in vs
        name = getindex(idx_to_name, v)
        name = getindex(name_map, name)
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

function viz!(c::CompositeComponent)
    g = convert_to_catlab_graph(c)
    CircuitViz.compile!(g)
end
