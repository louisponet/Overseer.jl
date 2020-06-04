#To implement AbstractLedger interface, the subtype should just define the function ledger
#where all the fields are like in this one

"""
    Basic Ledger. This takes care of creating and destroying entities,
    making sure all the requested components for the systems are initialized,
    and updates the systems.
"""
mutable struct Ledger <: AbstractLedger
	entities     ::Vector{Entity}
	free_entities::Vector{Entity}
	to_delete    ::Vector{Entity}
	components   ::Vector{Union{Component,SharedComponent}}
	groups       ::Vector{AbstractGroup}
	# components   ::Dict{DataType, Union{Component,SharedComponent}}

	stages::Vector{Stage}
end
Ledger() = Ledger(Entity[],
                    Entity[],
                    Entity[],
                    Union{Component,SharedComponent}[],
                    AbstractGroup[],
                    Pair{Symbol, Vector{System}}[])

function Ledger(comps::Vector{Union{Component, SharedComponent}})
    out = Ledger()
    out.components = comps
    return out 
end

function Ledger(cs::AbstractComponent...)
    compids = map(x->component_id(eltype(x)), cs)
	maxid = maximum(compids)

	comps = Vector{Union{Component, SharedComponent}}(undef, maxid)
	for (c, id) in zip(cs, compids)
    	comps[id] = c
	end
	for i = 1:maxid
    	if !isassigned(comps, i)
        	comps[i] = EMPTY_COMPONENT
    	end
	end
	return Ledger(comps)
end

Ledger(components::Type{<:ComponentData}...) = Ledger(map(x -> component_type(x){x}(), components)...)

function Ledger(stages::Stage...)
	comps = Type{<:ComponentData}[] 
	for stage in stages
		append!(comps, requested_components(stage)) 
	end
	m = Ledger(comps...)
	m.stages=[stages...]
	prepare(m)
	return m
end

ledger(m::Ledger) = m

components(m::AbstractLedger)       = ledger(m).components
entities(m::AbstractLedger)         = ledger(m).entities
free_entities(m::AbstractLedger)    = ledger(m).free_entities
to_delete(m::AbstractLedger)        = ledger(m).to_delete
valid_entities(m::AbstractLedger)   = filter(x -> x.id != 0, entities(m))
stages(m::AbstractLedger)           = ledger(m).stages
stage(m::AbstractLedger, s::Symbol) = ledger(m).stages[s]
groups(m::AbstractLedger)           = ledger(m).groups
singleton(m::AbstractLedger, ::Type{T}) where {T<:ComponentData} = m[T][1]

##### BASE Extensions ####
function Base.in(::Type{R}, m::AbstractLedger) where {R<:ComponentData}
    cid = component_id(R)
    return cid <= length(components(m)) && components(m)[cid] !== EMPTY_COMPONENT
end

function Base.empty!(m::AbstractLedger)
	empty!(entities(m))
	empty!(free_entities(m))
	empty!(to_delete(m))
	empty!(components(m))
	empty!(stages(m))
	empty!(groups(m))
end

function Base.getindex(m::AbstractLedger, ::Type{T}) where {T<:ComponentData}
	id = component_id(T)
	return components(m)[id]::component_type(T){T}
end

Base.copy(m::AbstractLedger) = Ledger(copy(entities(m)),
                                      copy(free_entities(m)),
                                      copy(to_delete(m)),
                                      [c === EMPTY_COMPONENT ? c : deepcopy(c) for c in components(m)],
                                      deepcopy(groups(m)), 
                                      deepcopy(stages(m)))

function Base.getindex(m::AbstractLedger, e::Entity)
	entity_assert(m, e)		
	data = ComponentData[]
	for c in components(m)
		if in(e, c)
			push!(data, c[e])
		end
	end
	return data
end

function Base.getindex(v::Vector{Stage}, s::Symbol)
    id = findfirst(x->first(x) == s, v)
    if id === nothing
        error("Stage $s not found.")
    end
    return v[id]
end

function Base.setindex!(m::AbstractLedger, v::T, e::Entity) where {T<:ComponentData}
	entity_assert(m, e)
	ensure_component!(m, T)
	if !in(e, m[T])
        m[T][e] = v
        register_new!(m, T, e)
        return v
    end
	return m[T][e] = v
end

function register_new!(m::AbstractLedger, ::Type{T}, e::Entity) where {T<:ComponentData}
    for g in groups(m)
        if !(g isa OrderedGroup)
            continue
        elseif T in g
            register_new!(find_lowest_child(g, T), e)
            return
        end
    end
end

function ensure_component!(m::AbstractLedger, c::Type{<:ComponentData})
    if !(c in m)
        m_comps = components(m)
        id = component_id(c)
        #I think this is never possible
        while id > length(m_comps)
            push!(m_comps, EMPTY_COMPONENT)
        end
        comp = component_type(c){c}()
        m_comps[id] = comp
    end
end

function Base.push!(m::AbstractLedger, stage::Stage)
    comps = requested_components(stage)
    for c in comps
        ensure_component!(m, c)
    end
    push!(stages(m), stage)
    prepare(stage, m)
end

function Base.insert!(m::AbstractLedger, i::Integer, stage::Stage)
    comps = requested_components(stage)
    for c in comps
        ensure_component!(m, c)
    end
    insert!(stages(m), i, stage)
    prepare(stage, m)
end

function Base.push!(m::AbstractLedger, s::Symbol, sys::System)
	st = stage(m, s) 
    comps = requested_components(sys)
    for c in comps
        ensure_component!(m, c)
    end
	push!(st, sys)
    prepare(sys, m)
end

function Base.insert!(m::AbstractLedger, s::Symbol, i::Int, sys::System)
	insert!(stage(m, s), i, sys)
    comps = requested_components(sys)
    for c in comps
        ensure_component!(m, c)
    end
    prepare(sys, m)
end

function Base.delete!(m::AbstractLedger, e::Entity)
	entity_assert(m, e)
	push!(free_entities(m), e)
	entities(m)[e.id] = EMPTY_ENTITY
	for c in components(m)
		if in(e, c)
			pop!(c, e)
		end
	end
end

function empty_entities!(m::AbstractLedger)
	empty!(entities(m))
	empty!(free_entities(m))
	for c in components(m)
    	empty!(c)
	end
end

function components(ledger::AbstractLedger, ::Type{T}) where {T<:ComponentData}
	comps = AbstractComponent[]
	for c in components(ledger)
		if eltype(c) <: T
			push!(comps, c)
		end
	end
	return comps
end

function entity_assert(m::AbstractLedger, e::Entity)
	es = entities(m)
	@assert length(es) >= e.id "$e was never initiated."
	@assert es[e.id] != EMPTY_ENTITY "$e was removed previously."
end

function schedule_delete!(m::AbstractLedger, e::Entity)
	entity_assert(m, e)
	push!(to_delete(m), e)
end

function delete_scheduled!(m::AbstractLedger)
	for c in components(m)
		delete!(c, to_delete(m))
	end
	for e in to_delete(m)
		entities(m)[e.id] = EMPTY_ENTITY
		push!(free_entities(m), e)
	end
end

function update(s::Stage, m::AbstractLedger)
    for s in last(s)
        update(s, m)
    end
end

function update(m::AbstractLedger)
	for stage in stages(m)
		update(stage, m)
	end
end

function prepare(m::AbstractLedger)
	for s in stages(m)
		prepare(s, m)
	end
end
