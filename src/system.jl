update(::S, m::AbstractLedger) where {S<:System}= error("No update method implemented for $S")

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

const Stage = Pair{Symbol, Vector{System}}

Base.push!(s::Stage, sys) = push!(last(s), sys)

Base.insert!(s::Stage, i::Integer, sys) = insert!(last(s), i, sys)

function requested_components(stage::Stage)
    comps = Type[]
	for s in last(stage)
		for c in requested_components(s)
			push!(comps, c)
		end
	end
	return comps
end

function prepare(s::Stage, m::AbstractLedger)
    for sys in last(s)
        prepare(sys, m)
    end
end

prepare(::System, ::AbstractLedger) = nothing
