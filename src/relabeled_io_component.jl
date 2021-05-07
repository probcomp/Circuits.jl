"""
    RelabeledIOComponent(component::Component, input_relabels, output_relabels, inputs_to_outputs=(); abstact::Union{Component, Nothing}=nothing)

Wraps `component` to reroute inputs according to `input_relabels`, reroute outputs according to `output_relabels`,
and pass inputs directly to outputs according to `inputs_to_outputs`.

- `input_relabels` should be an iterator over pairs `old_name => new_name`.
- `output_relabels` should be an iterator over pairs `old_name => new_name_or_names`, where
`new_name_or_names` is either a new name (a Symbol), an iterator over new names, or `nothing`.  If it is a Symbol,
`old_name` will be rerouted to `new_name_or_names`; if it is an iterator, `old_name` will be rerouted
to each element of the iterator; if it is `nothing`, this output will not be output from the relabeled component.
- `inputs_to_outputs` should be an iterator over pairs `new_in_name => new_out_name`, designating
which of the inputs to the relabeled component should be output to which output.

Any inputs or outputs of the original component which do not appear as an `old_name` in `input_relabels`/`output_relabels`
will be input/output from the relabeled component under their original names.

If given an `abstract`, takes `abstract` to be the abstract version of this component.
"""
struct RelabeledIOComponent <: GenericComponent
    component::Component
    input_relabels::Dict
    output_relabels::Dict
    input_to_output::Dict
    abstract::Union{Nothing, Component}
    RelabeledIOComponent(c, i, o, i_to_o=(); abstract=nothing) = new(c, Dict(i...), Dict(o...), Dict(i_to_o...), abstract)
end
relabeled_keys(k, relabels) =
    let key_or_keys = get(relabels, k, k)
        if key_or_keys isa Symbol
            (key_or_keys,)
        else
            @assert key_or_keys isa Tuple || key_or_keys isa Nothing
            key_or_keys
        end
    end
relabeled_key(args...) = only(relabeled_keys(args...))
function Circuits.inputs(c::RelabeledIOComponent)
    @assert inputs(c.component).vals isa NamedTuple || isempty(c.input_relabels) "Current implementation can only relabel symbol (not Int)-addressed values"
    return CompositeValue((;(
        relabeled_key(k, c.input_relabels) => v
        for (k, v) in pairs(inputs(c.component).vals)
    )...))
end
function outputs(c::RelabeledIOComponent)
    @assert outputs(c.component).vals isa NamedTuple || isempty(c.output_relabels) "Current implementation can only relabel symbol (not Int)-addressed values"
    return CompositeValue((; (
        ( # Relabelled outputs
            new_key => v
            for (k, v) in pairs(outputs(c.component).vals)
                for new_key in relabeled_keys(k, c.output_relabels)
                    if !isnothing(relabeled_keys(k, c.output_relabels))
        )...,
        ( # Outputs from inputs
            new_outname => inputs(c)[new_inname]
            for (new_inname, new_outname) in c.input_to_output
        )...
    )...))
end
implement(c::RelabeledIOComponent, ::Target) = CompositeComponent(
    inputs(c), outputs(c),
    (component=c.component,),
    Iterators.flatten((
        ( # input relabels
            Input(relabeled_key(k, c.input_relabels)) => CompIn(:component, k)
            for k in keys(inputs(c.component))
        ),
        ( # output relabels
            CompOut(:component, k) => Output(new_key)
            for k in keys(outputs(c.component))
                for new_key in relabeled_keys(k, c.output_relabels)
                    if !isnothing(relabeled_keys(k, c.output_relabels))
        ),
        ( # inputs to outputs
            Input(inname) => Output(outname)
            for (inname, outname) in c.input_to_output
        )
    )),
    c
)
abstract(c::RelabeledIOComponent) = c.abstract
target(c::RelabeledIOComponent) = target(c.component)