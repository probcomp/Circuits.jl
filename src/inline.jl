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

# TODO: usage of an IdDict as the work queue
# means that PathEdge instances might over-write each other -- especially at the toplevel component.
#
# This occurred previously when investigating a bug
# with halting the algorithm at toplevel Input
# ports.
# For example, we might image:
# () => (:t11, :neuron, 1)
# --> CompOut(sync, 2 => 1)
# () => (:t11, :neuron, 1)
# --> CompOut(sync, 1 => 1)
#
# Here, the PathEdge instances are the same -- but they are
# fed by separate CompOut ports.
#
# Ask George about this.

mutable struct InlineState
    c::CompositeComponent
    completed_paths::Vector{PathEdge}
    primitives::Dict{Tuple, Any}
    pmap::IdDict{PathEdge, Any}
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
    s = sort(ds.completed_paths; by = x -> length(x.current))
    for p in s
        println("$(p.current => p.start)")
    end
    println()
    println("Current work:")
    for k in keys(ds.pmap)
        work = ds.pmap[k]
        println("$(k.current => k.start)\n --> $(work)")
    end
end

function prepare(c::CompositeComponent)
    out_iter = output_iterator(c)
    pmap = IdDict(map(out_iter) do ind
                      map(inputters(c, ind)) do send
                          PathEdge(ind, send) => send
                      end
                  end |> Iterators.flatten)
    return InlineState(c, 
                        PathEdge[], 
                        Dict{Tuple, Vector{Any}}(), 
                        pmap,
                        Tuple[])
end

# Iterative BF traversal.
function step!(state::InlineState)
    c = state.c
    subcomps = c.subcomponents
    new_work = Pair{PathEdge, Any}[]
    for (k, v) in state.pmap
        append!(new_work, step!(c, k, v, state))
    end
    for path in new_work
        cur, port = path
        setindex!(state.pmap, port, cur)
    end
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

# Here, we are moving from an inner CompositeComponent
# to the next nesting level.
# Therefore, the new work needs to be created:
# 1. Turn the Input into a CompIn.
# 2. Get all the senders to that CompIn.
# 3. Add those senders to the work queue.
function handle_input!(subcomp::CompositeComponent, 
        par::PathEdge, inp::Input, 
        state::InlineState)
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
            push!(state.completed_paths, finished)
            setindex!(state.primitives, subcomp, finished.current)
        else
            push!(new_work, new_edge => send)
        end
    end
    delete!(state.pmap, par)
    return new_work
end

function handle_compin!(subcomp, par::PathEdge, 
        comp_in::CompIn, state::InlineState)
    senders = inputters(subcomp, comp_in) |> collect
    new_work = []
    for send in senders
        push!(new_work, par => send)
    end
    delete!(state.pmap, par)
    return new_work
end

# This means -- we've hit a primitive.
# There's no other way to hit a primitive.
function handle_compout!(subcomp, 
        par::PathEdge, comp_out::CompOut,
        state::InlineState)
    finished = PathEdge(par.start,
                        (par.current..., comp_out.comp_name,
                         comp_out.out_name))
    push!(state.completed_paths, finished)
    setindex!(state.primitives, subcomp, finished.current)
    delete!(state.pmap, par)

    # If we've already encountered this primitive before, 
    # we've already added edges from this node before.
    # Return immediately with no new work edges.
    component_addr = (par.current..., comp_out.comp_name)
    check_mark(state, component_addr) && return []

    # Else, we need to add new edges.
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
        setindex!(state.primitives, subcomp, port)
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

# This is handling an PathEdge from a parent CompositeComponent
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
    delete!(state.pmap, par)
    return new_work
end

function step!(c::CompositeComponent, 
        par::PathEdge, v::CompOut, 
        state::InlineState)
    subcomp_name = (par.current..., v.comp_name)
    subcomp = getindex_recurse(c, subcomp_name)
    new_work = handle_compout!(subcomp, par, v, state)
    return new_work
end

# This should only happen when leaving a primitive component.
# Ask for the senders to a CompIn to a primitive component.
# May return an edge for an Input of the composite component.
# May also return an edge of a CompIn inside the 
# composite component.
function step!(c::CompositeComponent,
        par::PathEdge, v::Input,
        state::InlineState)
    subcomp_name = par.current[1 : end - 1]
    subcomp = getindex_recurse(c, subcomp_name)
    new_work = handle_input!(subcomp, par, v, state)
    return new_work
end

function step!(c::CompositeComponent, 
        par::PathEdge, v::CompIn, 
        state::InlineState)
    subcomp_name = par.current
    subcomp = getindex_recurse(c, subcomp_name)
    new_work = handle_compin!(subcomp, par, v, state)
    return new_work
end

function inline(c::CompositeComponent)
    state = prepare(c)
    while !isempty(state.pmap)
        step!(state)
    end
    return state
end
