function add_node!(g, name::Symbol, subcomp::InputOutputComponent)
    node_attrs = Dict(:shape => "circle",
                      :label => "$(name)")
    CircuitViz.add_node!(g, node_attrs)
end

function add_node!(g, name::Symbol, subcomp::PrimitiveComponent)
    node_attrs = Dict(:shape => "triangle",
                      :label => "$(name)")
    CircuitViz.add_node!(g, node_attrs)
end

function add_node!(g, name::Symbol, subcomp::CompositeComponent)
    node_attrs = Dict(:shape => "box",
                      :label => "$(name)")
    CircuitViz.add_node!(g, node_attrs)
end

function convert_to_catlab_graph(c::CompositeComponent)
    g, idx_to_name, name_to_idx = _subcomponent_graph(c)
    vs = vertices(g)
    es = edges(g)
    for v in vs
        name = getindex(idx_to_name, v)
        subcomp = getindex(c, name)
        add_node!(g, name, subcomp) # This handles properties using dispatch.
    end
    for e in es
        src = e.src
        dst = e.dst
        add_edge!(g, src, dst)
    end
    return g
end

function viz!(c::CompositeComponent)
    g = convert_to_catlab_graph(c)
    CircuitViz.compile!(g)
end
