"""
    update(system, ledger, args...)

Function to be overloaded for any [`System`](@ref) which will be called during the `update` call stack.
"""
update(::S, l::AbstractLedger, args...) where {S<:System}= error("No update method implemented for $S")

"""
    requested_components(::System)

Function to be overloaded so that when a [`Ledger`](@ref) is created containing
a system, the right [`AbstractComponents`](@ref AbstractComponent) will be
added to it.

# Examples
```julia
@component struct ExampleComp end

struct ExampleSystem <: System end

Overseer.requested_components(::ExampleSystem) = (ExampleComp,)

l = Ledger(Stage(:example, [ExampleSystem()]))
```
"""
requested_components(::System) = ()

"""
Represents a set of [`Systems`](@ref System) that get executed as steps by calling `update` on those systems.
The steps are a `Vector` of other `Stages`, `Systems`, or a `Vector` of those.
During the [`update`](@ref update(::Stage, ::AbstractLedger)) call on a `Stage`, if one of the steps is found to be a `Vector` it will be assumed
that those can be executed at the same time (see the example). The representation of the steps can be thought of as
a very crude `DAG`.

# Example

```julia
Stage(:example_stage, [Sys1, [Sys2, Sys3, Sys4], Sys5])
```
When `update` is called on this stage, first the [`update`](@ref update(::System, ::AbstractLedger)) of `Sys1` will be called, then
3 tasks will be spawned each calling `update` for `Sys2`, `Sys3` and `Sys4` at the same time. Finally after those complete the
`update` function of `Sys5` will be called.
"""
struct Stage
    name::Symbol
    steps::Vector{Union{System, Stage, Vector}} 
end

Base.push!(s::Stage, step) = push!(s.steps, step)

Base.insert!(s::Stage, i::Integer, step) = insert!(s.steps, i, step)

function requested_components(systems::Vector)
    comps = Type[]
    for s in systems
        append!(comps, requested_components(s))
    end
    return comps
end
requested_components(stage::Stage) = requested_components(stage.steps)

function prepare(systems::Vector, l::AbstractLedger)
    for sys in systems
        prepare(sys, l)
    end
end
    
prepare(s::Stage, l::AbstractLedger) = prepare(s.steps, l)

prepare(::System, ::AbstractLedger) = nothing

"""
    update(stage, ledger, args...)

recursively calls `update` with the `ledger` on the steps defined in the `stage`.
See the [`Stage`](@ref) documentation for an explanation of the execution graph.
"""
function update(stage::Stage, l::AbstractLedger, args...)
    # Steps in a stage get executed in sequence, but if
    # a step is a vector they are threaded
    for step in stage.steps
        if step isa Vector
            Threads.@threads for t in step
                update(t, l, args...)
            end
        else
            update(step, l, args...)
        end
    end
end

function Base.in(s::System, stage::Stage)
    for step in stage.steps
        
        if step == s
            return true
        end
        
        if step isa Vector
            return any(x -> x isa System && x == s || s in x, step)
        end
        
    end
    return false
end
            
