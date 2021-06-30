#
# The flattening problem:
#
# Each CompositeComponent has some set of Inputs and Outputs.
# As well as some set of "inner" CompIns and CompOuts.
# 
# For each Input `I` of the composite component, there is some set of edges from `I` to CompIn (or directly to Output).
# If edge E is an edge from `I` to a CompIn `C_in`
# We must associate new edges from `I` to sub-components which are
# routed to by `C_in`.
#
# Let's consider a "1-level" flattening where we have:
#
#     |             _ _ _ _ _ _     |
#     |            |     prim  |    |
# (I) * -> (C_in)  *  -> |> -> * -> * (O)
#     |            |_ _ _ _ _ _|    |
#     |                             |
#
# The new edge for `I` must go to `prim` directly.
# 


# Defines "leaves" for `Base.iterate`.
# I suspect this is required for Iterators.flatten
# to work correctly.
import Base: iterate
Base.iterate(inp::Input) = (inp, nothing)
Base.iterate(inp::Input, n::Nothing) = n
Base.iterate(out::Output) = (out, nothing)
Base.iterate(out::Output, n::Nothing) = n
Base.iterate(inp::CompIn) = (inp, nothing)
Base.iterate(inp::CompIn, n::Nothing) = n

second(t::Tuple) = t[2 : end]

# Anything which is not a CompositeComponent is a leaf.
function _flatten(c)
    idx_to_in = collect(keys_deep(inputs(c)))
    idx_to_out = collect(keys_deep(outputs(c)))
    in_to_idx = Dict(v => k for (k, v) in enumerate(idx_to_in))
    out_to_idx = Dict(v => k for (k, v) in enumerate(idx_to_out))
    return (c, in_to_idx, out_to_idx)
end

function new_names(src::Input, 
        c, 
        flt_subs,
        subsubcomp_name_to_idx, 
        in_to_idx, 
        out_to_idx,
        subcomp_mappings)
    return (Input(in_to_idx[src.id]), )
end

function new_names(dest::Output, 
        c,
        flt_subs, 
        subsubcomp_name_to_idx, 
        in_to_idx, 
        out_to_idx,
        subcomp_mappings)
    return (Output(out_to_idx[dest.id]), )
end

function new_names(src::CompIn, 
        c,
        flt_subs, 
        subsubcomp_name_to_idx, 
        in_to_idx, 
        out_to_idx,
        subcomp_mappings)

    # Dumb debug.
    sleep(0.3)
    display(src)

    # Inverse map for outputs.
    d = Dict(v => k for (k, v) in subcomp_ms[2]) 
    comp_name = src.comp_name
    in_name = src.in_name
    subcomp = flt_subs[comp_name]
    subcomp_ms = subcomp_mappings[comp_name]
    indexed = []

    # Generate list of receivers from `src` and the subcomp
    # which `src` enters into.
    if subcomp isa PrimitiveComponent
        rec = map(Output, Base.keys(Circuits.outputs(subcomp)))
    else
        inmap = subcomp_ms[1]
        ind = inmap[in_name]
        rec = receivers(subcomp, Input(ind))
    end


    for r in rec

        # Two cases:

        # (1) CompIn -- we should check if the subcomp is a 
        # PrimitiveComponent. If it is, we construct the right CompIn
        # to reference the index + plus the internal in_name.
        # If it's not Primitive, it should already be the right
        # index.

        if r isa CompIn
            if subcomp isa PrimitiveComponent

                # Note that this should handle re-naming as we move
                # up from leaves of DFS automatically...
                subcomp_name = subsubcomp_name_to_idx[(src.comp_name, r.in_name)]
                push!(indexed, CompIn(subcomp_name, r.in_name))
            else

                # r should already correspond to a
                # primitive index.
                push!(indexed, r) 
            end

            # (2) Output -- this is the Output from a subcomponent
            # which means we need to get the receivers from
            # that Output in `c` -- the current component scope.
            # Now, if subcomp is a PrimitiveComponent -- we can use
            # the receiver id directly -- because this name hasn't 
            # been mapped to an index yet.
            # If the subcomp is not a PrimitiveComponent,
            # we need to use the output map from `subcomp_mappings`
            # to figure out what the correct name is. Below, this is
            # `d`.

        elseif r isa Output

            # If we're not working with a primitive component,
            # we should have already created an integer index.
            
            if !(subcomp isa PrimitiveComponent)
                key = d[r.id]
            else
                key = r.id
            end

            # Now we get all the receivers in the current
            # component scope. We iterate over receivers 
            # and bail out if we detect a cycle 
            # (e.g. a receiver is the same as the current
            # `src::CompIn`)
            # Otherwise, we call `new_names` and recurse.
            
            rec_in_c = receivers(c, CompOut(comp_name, key))
            for t in rec_in_c

                # We need to bail out of cycles...
                if t == src
                    subcomp_name = subsubcomp_name_to_idx[(src.comp_name, t.comp_name)]
                    push!(indexed, CompIn(subcomp_name, in_name))
                else
                    append!(indexed, new_names(t,
                                               c,
                                               flt_subs,
                                               subsubcomp_name_to_idx, 
                                               in_to_idx, 
                                               out_to_idx,
                                               subcomp_mappings)
                           )
                end
            end
        end
    end
    return Iterators.flatten(indexed)
end

function new_names(dest::CompOut, 
        c, 
        flt_subs,
        subsubcomp_name_to_idx, 
        in_to_idx, 
        out_to_idx,
        subcomp_mappings)
    comp_name = dest.comp_name
    out_name = dest.out_name
    subcomp = flt_subs[comp_name]
    subcomp_ms = subcomp_mappings[comp_name]
    if subcomp isa PrimitiveComponent
        inp = ()
    else
        o_map = subcomp_ms[2]
        k = o_map[out_name]
        inp = inputters(subcomp, Output(k))
    end
    indexed = map(inp) do r
        if r isa CompOut
            subcomp_name = subsubcomp_name_to_idx[(dest.comp_name, r.comp_name)]
            CompOut(subcomp_name, r.out_name)
        else
            ()
        end
    end
    return Iterators.flatten(indexed)
end

function flat_subsub(v)
    if v isa CompositeComponent
        return v.subcomponents
    else
        return (v, )
    end
end

function _flatten(c::CompositeComponent)
    flt_subs_and_mappings = map(_flatten, c.subcomponents)
    flt_subs = map(first, flt_subs_and_mappings)
    subcomp_mappings = map(second, flt_subs_and_mappings)
    # each element of subcomp_mappings is `input_map, output_map` giving the name
    idx_to_in = inputs(c) |> keys_deep |> collect
    idx_to_out = outputs(c) |> keys_deep |> collect
    in_to_idx = Dict(v => k for (k, v) in enumerate(idx_to_in))
    out_to_idx = Dict(v => k for (k, v) in enumerate(idx_to_out))
    idx_to_subsubcomp_name = []
    for (name, subcomp) in pairs(c.subcomponents)
        if subcomp isa CompositeComponent 
            for (innername, _) in pairs(subcomp.subcomponents)
                push!(idx_to_subsubcomp_name, (name, innername))
            end
        else
            push!(idx_to_subsubcomp_name, (name, name))
        end
    end
    subsubcomp_name_to_idx = Dict(v => k for (k, v) 
                                  in enumerate(idx_to_subsubcomp_name))
    new_edges = []
    for (src, dst) in get_edges(c)
        srcnames = new_names(src, 
                             c, 
                             flt_subs, 
                             subsubcomp_name_to_idx, 
                             in_to_idx, 
                             out_to_idx,
                             subcomp_mappings)

        dstnames = new_names(dst, 
                             c,
                             flt_subs, 
                             subsubcomp_name_to_idx, 
                             in_to_idx, 
                             out_to_idx,
                             subcomp_mappings)

        for (s, d) in Iterators.product(srcnames, dstnames)
            push!(new_edges, s => d)
        end
    end

    @info "Building new flat component..."
    display(new_edges)
    new_comp = CompositeComponent(
                                  IndexedValues([inputs(c)[name] for name in idx_to_in]),
                                  IndexedValues([outputs(c)[name] for name in idx_to_out]),
                                  Iterators.flatten(flat_subsub(v) for v in flt_subs) |> collect,
                                  new_edges)
    return (new_comp, in_to_idx, out_to_idx)
end

function flatten(c::CompositeComponent)
    new_comp, in_to_idx, out_to_idx = _flatten(c)
    return new_comp
end
