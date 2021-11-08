function output_iterator(c::CompositeComponent)
    outs = outputs(c) |> keys_deep
    return map(Output, outs)
end

struct PathEdge
    start::Tuple
    current::Tuple
end

import Base: display
function Base.display(pe::PathEdge)
    println("$(pe.current => pe.start)")
end

function PathEdge(out::Output, c_out::CompOut)
    return PathEdge((out.id, ), ())
end

function PathEdge(out::Output, c_out::Input)
    return PathEdge((out.id, ), ())
end

mutable struct InlineState
    c::CompositeComponent
    completed_edges::Vector{PathEdge}
    inputs::Dict{Tuple, Any}
    internals::Dict{Tuple, Any}
    outputs::Dict{Tuple, Any}
    primitive_nodes::Vector{Any}
    stack::Vector{Pair{PathEdge, Any}}
    marked::Vector{Tuple}
end

function mark!(state::InlineState, addr::Tuple)
    push!(state.marked, addr)
end

function check_mark(state::InlineState, addr::Tuple)
    return addr in state.marked
end

function Base.display(ds::InlineState)
    println("Completed edges:")
    s = sort(ds.completed_edges; by = x -> length(x.current))
    for p in s
        println("$(p.current => p.start)")
    end
    println()
    println("Current work:")
    for (path, work) in ds.stack
        println("$(path.current => path.start)\n --> $(work)")
    end
end

function prepare(c::CompositeComponent)
    out_iter = output_iterator(c)
    stack = map(out_iter) do ind
        map(inputters(c, ind)) do send
            PathEdge(ind, send) => send
        end
    end |> Iterators.flatten
    outputs = Dict(map(out_iter) do ind
        (ind.id, ) => ind
    end)
    stack = collect(stack)
    return InlineState(c, 
                       PathEdge[], 
                       Dict{Tuple, Any}(), 
                       Dict{Tuple, Any}(),
                       outputs,
                       Any[],
                       stack,
                       Tuple[])
end

# Iterative BF traversal.
function step!(state::InlineState; treat_as_primitive = s -> false)
    c = state.c
    subcomps = c.subcomponents
    new_work = Pair{PathEdge, Any}[]
    while !isempty(state.stack)
        edge, work = pop!(state.stack)
        append!(new_work, step!(c, edge, work, state, treat_as_primitive))
    end
    append!(state.stack, new_work)
    return state
end

function getindex_recurse(c, addr)
    return c
end

function getindex_recurse(c::CompositeComponent, addr::Tuple{})
    return c
end

function getindex_recurse(c::CompositeComponent, addr::Tuple)
    subcomps = c.subcomponents
    subcomp = getindex(subcomps, first(addr))
    return getindex_recurse(subcomp, addr[2 : end])
end

function handle_input!(subcomp::CompositeComponent, 
        par::PathEdge, inp::Input, 
        state::InlineState)
    # Check if at top level.
    if isempty(par.current)
        finished = PathEdge(par.start,
                            (inp.id, ))
        push!(state.completed_edges, finished)
        setindex!(state.inputs, inp, 
                  finished.current)
        return []
    end

    # Here, we are moving from an inner CompositeComponent
    # to the next nesting level.
    # Therefore, the new work needs to be created:
    # 1. Turn the Input into a CompIn.
    # 2. Get all the senders to that CompIn.
    # 3. Add those senders to the work queue.
    comp_in = CompIn(par.current[end], inp.id)
    new_edge = PathEdge(par.start, par.current[1 : end - 1])
    subcomps = state.c.subcomponents
    subcomp = getindex_recurse(state.c, 
                               par.current[1 : end - 1])
    senders = inputters(subcomp, comp_in) |> collect
    new_work = []
    for send in senders
        # If the current address is null, you're at
        # the toplevel.
        # If the sender is also an Input, that means you've
        # reached a toplevel Input.
        # Add this path to finished.
        if isempty(new_edge.current) &&
            send isa Input
            finished = PathEdge(new_edge.start,
                                (send.id, ))
            push!(state.completed_edges, finished)
            setindex!(state.inputs, send, 
                      finished.current)
        else
            push!(new_work, new_edge => send)
        end
    end
    return new_work
end

function handle_compin!(subcomp, par::PathEdge, 
        comp_in::CompIn, state::InlineState)
    senders = inputters(subcomp, comp_in) |> collect
    new_work = []
    for send in senders
        push!(new_work, par => send)
    end
    return new_work
end

# This means -- we've hit a primitive.
# There's no other way to hit a primitive.
function handle_compout_primitive!(subcomp, 
        par::PathEdge, comp_out::CompOut,
        state::InlineState)
    finished = PathEdge(par.start,
                        (par.current..., comp_out.comp_name,
                         comp_out.out_name))
    push!(state.completed_edges, finished)
    primitive = subcomp

    # Check -- have we already added this primitive to the
    # primitive vector.
    # We may have already encountered this primitive
    # before.
    component_addr = (par.current..., comp_out.comp_name)
    check_mark(state, component_addr) && return []
    push!(state.primitive_nodes, primitive)
    index = length(state.primitive_nodes)
    setindex!(state.internals, index, finished.current)

    # We need to add new edges.
    recs = inputs(subcomp) |> keys_deep
    recs = map(v -> CompIn(comp_out.comp_name, v), recs)

    # We now want to get the subcomponent which contains
    # the primitive.
    c = state.c
    subcomp_name = par.current
    subcomp = getindex_recurse(state.c, subcomp_name)
    senders = []
    for r in recs
        base = par.current
        port = (base..., comp_out.comp_name, r.in_name)
        setindex!(state.internals, index, port)
        for send in inputters(subcomp, r)
            p = PathEdge(port, port[1 : end - 2])
            push!(senders, p => send)
        end
    end

    # We now mark the primitive as having added all CompOut
    # edges to the work queue -- this prevents
    # adding new edges to work on if we encounter
    # this primitive again, e.g. in a cycle.
    mark!(state, component_addr)
    return senders
end

# This is handling a PathEdge from a parent CompositeComponent
# to a CompOut which comes from a nested CompositeComponent.
function handle_compout!(subcomp::CompositeComponent, 
        par::PathEdge, comp_out::CompOut,
        state::InlineState)
    out_name = comp_out.out_name
    senders = inputters(subcomp, Output(out_name)) |> collect
    new_work = []
    new_edge = PathEdge(par.start, 
                        (par.current..., comp_out.comp_name))
    for send in senders
        push!(new_work, new_edge => send)
    end
    return new_work
end

# Fallback to primitive.
function handle_compout!(subcomp, par::PathEdge, 
        comp_out::CompOut, state::InlineState)
    return handle_compout_primitive!(subcomp, par, comp_out, state)
end

function check_recurse_compout!(subcomp, par::PathEdge, 
        comp_out::CompOut, state::InlineState, treat_as_primitive)
    if treat_as_primitive(subcomp)
        return handle_compout_primitive!(subcomp, par, comp_out, state)
    else
        return handle_compout!(subcomp, par, comp_out, state)
    end
end

function step!(c::CompositeComponent, 
        par::PathEdge, v::CompOut, 
        state::InlineState, treat_as_primitive)
    subcomp_name = (par.current..., v.comp_name)
    subcomp = getindex_recurse(c, subcomp_name)
    new_work = check_recurse_compout!(subcomp, par, v, state, treat_as_primitive)
    return new_work
end

# This should only happen when leaving a primitive component.
# Ask for the senders to a CompIn to a primitive component.
# May return an edge for an Input of the composite component.
# May also return an edge of a CompIn inside the 
# composite component.
function step!(c::CompositeComponent,
        par::PathEdge, v::Input,
        state::InlineState, treat_as_primitive)
    subcomp_name = par.current[1 : end - 1]
    subcomp = getindex_recurse(c, subcomp_name)
    new_work = handle_input!(subcomp, par, v, state)
    return new_work
end

function step!(c::CompositeComponent, 
        par::PathEdge, v::CompIn, 
        state::InlineState, treat_as_primitive)
    subcomp_name = par.current
    subcomp = getindex_recurse(c, subcomp_name)
    new_work = handle_compin!(subcomp, par, v, state)
    return new_work
end

function _inline(c::CompositeComponent; treat_as_primitive = s -> false)
    state = prepare(c)
    while !isempty(state.stack)
        step!(state; treat_as_primitive = treat_as_primitive)
    end
    return state
end

function create_new_edges_from_state(state::InlineState)
    map(state.completed_edges) do p
        start = p.start
        finish = p.current
        if length(start) == 1
            tg = getindex(state.outputs, start)
        else
            tg = getindex(state.internals, start)
            tg = CompIn(tg, start[end])
        end
        if length(finish) == 1
            src = getindex(state.inputs, finish)
        else
            src = getindex(state.internals, finish)
            src = CompOut(src, finish[end])
        end
        src => tg
    end
end

function create_cc_from_state(state::InlineState)
    original = state.c
    internal_nodes = state.primitive_nodes
    internal_edges = create_new_edges_from_state(state)
    return CompositeComponent(
                              original.input,
                              original.output,
                              state.primitive_nodes,
                              internal_edges,
                             )
end

function inline(c::CompositeComponent; treat_as_primitive = s -> false)
    state = _inline(c; treat_as_primitive = treat_as_primitive)
    names = Dict(v => k[1 : end - 1] 
                 for (k, v) in state.internals)
    return create_cc_from_state(state), names, state
end
