update(::S, l::AbstractLedger) where {S<:System}= error("No update method implemented for $S")

"""
    requested_components(::System)

Function to be overloaded so that when a [`Ledger`](@ref) is created containing
a system, the right [`AbstractComponents`](@ref AbstractComponent) will be
added to it.

# Examples
```jldoctest
julia> @component struct ExampleComp end

julia> struct ExampleSystem <: System end

julia> Overseer.requested_components(::ExampleSystem) = (ExampleComp,)

julia> l = Ledger(Stage(:example, [ExampleSystem()]))
Ledger
Components:
        0-element Component{ExampleComp}
        Total entities: 0
```
"""
requested_components(::System) = ()

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
            
