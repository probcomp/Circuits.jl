"""
A component representing an input to or output from a component.
(We can think of this as a sensor or actuator.)

Currently this is only used for visualizing circuits; if we wish to
use it in other ways in the future we may want to think through this more carefully.
"""
struct InputOutputComponent <: Component
    v::Value
end
inputs(c::InputOutputComponent) = NamedValues(:in => c.v)
outputs(c::InputOutputComponent) = NamedValues(:out => c.v)

"""
Converts a CompositeComponent with inputs and outputs to a CompositeComponent
with no input or output values, but with `InputOutputComponent`s in place
of each of the input/output value nodes from the original component.
"""
inputs_outputs_to_components(c::CompositeComponent) =
    CompositeComponent(
        inputs(c), outputs(c),
        (;
            original_subcomponents=ComponentGroup(c.subcomponents),
            input_nodes=nodes_for_value(inputs(c)),
            output_nodes=nodes_for_value(outputs(c))
        ),
        (
            (update_original_edge(src, dst) for (src, dst) in get_edges(c))...,
            io_node_input_edges(c)..., io_node_output_edges(c)...
        )
    )

nodes_for_value(v::Value) = InputOutputComponent(v)
nodes_for_value(v::CompositeValue) = ComponentGroup(map(nodes_for_value, v.vals))

update_original_edge(src, dst) = update_src(src) => update_dst(dst)
update_src(n::Input) = CompOut(:input_nodes, nest(valname(n), :out))
update_src(::Union{Output, CompIn}) = error() # should never happen
update_src(n::CompOut) = CompOut(:original_subcomponents, n.comp_name => valname(n))

update_dst(n::Output) = CompIn(:output_nodes, nest(valname(n), :in))
update_dst(::Union{Input, CompOut}) = error() # should never happen
update_dst(n::CompIn) = CompIn(:original_subcomponents, n.comp_name => valname(n))

io_node_input_edges(c) = (
    Input(i) => CompIn(:input_nodes => i, :in) for i in keys_deep(inputs(c))
)
io_node_output_edges(c) = (
    CompOut(:output_nodes => o, :out) => Output(o) for o in keys_deep(outputs(c))
)