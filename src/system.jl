update(::S, m::AbstractOverseer) where {S<:System}= error("No update method implemented for $S")

requested_components(::System) = ()

const SystemStage = Pair{Symbol, Vector{System}}

Base.push!(s::SystemStage, sys) = push!(last(s), sys)

Base.insert!(s::SystemStage, i::Integer, sys) = insert!(last(s), i, sys)

function requested_components(stage::SystemStage)
    comps = Type{<:ComponentData}[]
	for s in last(stage)
		for c in requested_components(s)
			push!(comps, c)
		end
	end
	return comps
end

function prepare(s::SystemStage, m::AbstractOverseer)
    for sys in last(s)
        prepare(sys, m)
    end
end

prepare(::System, ::AbstractOverseer) = nothing
