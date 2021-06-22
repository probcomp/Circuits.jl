# The flattening algorithm:
#
# Each CompositeComponent has some set of Inputs and Outputs.
# As well as some set of "inner" CompIns and CompOuts.
# 
# The algorithm is recursive. 
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
# Interface functions used:
# 
# - inputters: for a `NodeName` name (which is the name of a component in a larger component), 
# iterator over all other `NodeName` instances which send an input to name.
# 
# - receivers: for a `NodeName` name, iterator over all other `NodeName` instance which receive
# from name.

# TODO:
# 1. Test case: simple one (no recurrence)
# 2. Test case: simple recurrence.
# 3. Test case: explicit passthrough in subcomponent.
# 4. Test case: passthrough which then recurs.
#         - (goes back in for a finite number of cycles).
#
# TODO: also output some data structure which lets us lookup
# the nested name for a neuron before flattening.

second(t::Tuple) = t[2 : end]

# Leaf.
function flatten(c)
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
        out_to_idx)
    return (in_to_idx[src.id], )
end

function new_names(dest::Output, 
        flt_subs, 
        subsubcomp_name_to_idx, 
        in_to_idx, 
        out_to_idx, 
        subcomp_mappings)
    return (out_to_idx[dest.id], )
end

function new_names(src::CompIn, 
        c,
        flt_subs, 
        subsubcomp_name_to_idx, 
        in_to_idx, 
        out_to_idx)
    comp_name = src.comp_name
    in_name = src.in_name
    subcomp = flt_subs[comp_name]
    rec = receivers(subcomp, Input(in_name))
    indexed = map(rec) do r
        if r isa CompIn
            subcomp_name = subsubcomp_name_to_idx[(src.comp_name, r.comp_name)]
            CompIn(subcomp_name, r.in_name)
        elseif r isa Output
            rec_in_c = receivers(c, CompOut(subcomp_name, r.id))
            map(rec_in_c) do target
                new_names(target,
                          c,
                          flt_subs,
                          subsubcomp_name_to_idx, 
                          in_to_idx, 
                          out_to_idx)
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
        out_to_idx)
    comp_name = dest.comp_name
    out_name = dest.out_name
    subcomp = flt_subs[comp_name]
    inp = inputters(subcomp, Output(in_name))
    indexed = map(inp) do r
        if r isa CompOut
            subcomp_name = subsubcomp_name_to_idx[(dest.comp_name, r.comp_name)]
            CompOut(subcomp_name, r.in_name)
        else
            ()
        end
    end
    return Iterators.flatten(indexed)
end

function flatten(c::CompositeComponent)
    flt_subs_and_mappings = map(flatten, c.subcomponents)
    flt_subs = map(first, flt_subs_and_mappings)
    subcomp_mappings = map(second, flt_subs_and_mappings)
    # each element of subcomp_mappings is `input_map, output_map` giving the name
    # maps for a subcomponent
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
                             out_to_idx)

        dstnames = new_names(dst, 
                             c,
                             flt_subs, 
                             subsubcomp_name_to_idx, 
                             in_to_idx, 
                             out_to_idx)

        for (s, d) in Iterators.product(srcnames, dstnames)
            push!(new_edges, s => d)
        end
    end
    new_comp = CompositeComponent(
            IndexedValues([inputs(c)[name] for name in idx_to_in]),
            IndexedValues([outputs(c)[name] for name in idx_to_out]),
                                  flt_subs,
                                  new_edges
                                 )
    return (new_comp, in_to_idx, out_to_idx)
end
