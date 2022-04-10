update(::S, m::AbstractLedger) where {S<:System}= error("No update method implemented for $S")

requested_components(::System) = ()
in_components(::System) = ()

const Stage = Pair{Symbol, Vector{System}}

Base.push!(s::Stage, sys) = push!(last(s), sys)

Base.insert!(s::Stage, i::Integer, sys) = insert!(last(s), i, sys)

function requested_components(stage::Stage)
    comps = Type{<:ComponentData}[]
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
