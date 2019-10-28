#To implement AbstractOverseer interface, the subtype should just define the function manager
#where all the fields are like in this one

"""
    Basic Overseer. This takes care of creating and destroying entities,
    making sure all the requested components for the systems are initialized,
    and updates the systems.
"""
mutable struct Overseer <: AbstractOverseer
	entities     ::Vector{Entity}
	free_entities::Vector{Entity}
	to_delete    ::Vector{Entity}
	components   ::Vector{Union{Component,SharedComponent}}
	groups       ::Vector{AbstractGroup}
	# components   ::Dict{DataType, Union{Component,SharedComponent}}

	system_stages::Vector{SystemStage}
end
Overseer() = Overseer(Entity[],
                    Entity[],
                    Entity[],
                    Union{Component,SharedComponent}[],
                    AbstractGroup[],
                    Pair{Symbol, Vector{System}}[])

function Overseer(comps::Vector{Union{Component, SharedComponent}})
    out = Overseer()
    out.components = comps
    return out 
end

function Overseer(cs::AbstractComponent...)
	maxid = length(COMPONENTDATA_TYPES)

	comps = Vector{Union{Component, SharedComponent}}(undef, maxid)
	for c in cs
    	comps[component_id(eltype(c))] = c
	end
	for i = 1:maxid
    	if !isassigned(comps, i)
        	comps[i] = EMPTY_COMPONENT
    	end
	end
	return Overseer(comps)
end

Overseer(components::Type{<:ComponentData}...) = Overseer(map(x -> component_type(x){x}(), components)...)

function Overseer(system_stages::SystemStage...)
	comps = Type{<:ComponentData}[] 
	for stage in system_stages
		append!(comps, requested_components(stage)) 
	end
	m = Overseer(comps...)
	m.system_stages=[system_stages...]
	prepare(m)
	return m
end

manager(m::Overseer)                                               = m

components(m::AbstractOverseer)                                    = manager(m).components
entities(m::AbstractOverseer)                                      = manager(m).entities
free_entities(m::AbstractOverseer)                                 = manager(m).free_entities
to_delete(m::AbstractOverseer)                                     = manager(m).to_delete
valid_entities(m::AbstractOverseer)                                = filter(x -> x.id != 0, entities(m))
system_stages(m::AbstractOverseer)                                 = manager(m).system_stages
system_stage(m::AbstractOverseer, s::Symbol)                       = manager(m).system_stages[s]
singleton(m::AbstractOverseer, ::Type{T}) where {T<:ComponentData} = m[T][1]
groups(m::AbstractOverseer) = manager(m).groups

##### BASE Extensions ####
function Base.in(::Type{R}, m::AbstractOverseer) where {R<:ComponentData}
    cid = component_id(R)
    return cid <= length(components(m)) && components(m)[cid] !== EMPTY_COMPONENT
end

function Base.empty!(m::AbstractOverseer)
	empty!(entities(m))
	empty!(free_entities(m))
	empty!(to_delete(m))
	empty!(components(m))
	empty!(system_stages(m))
	empty!(groups(m))
end

function Base.getindex(m::AbstractOverseer, ::Type{T}) where {T<:ComponentData}
	id = component_id(T)
	return components(m)[id]::component_type(T){T}
end

function Base.getindex(m::AbstractOverseer, e::Entity)
	entity_assert(m, e)		
	data = ComponentData[]
	for c in components(m)
		if in(e, c)
			push!(data, c[e])
		end
	end
	return data
end

function Base.getindex(v::Vector{SystemStage}, s::Symbol)
    id = findfirst(x->first(x) == s, v)
    if id === nothing
        error("Stage $s not found.")
    end
    return v[id]
end

function Base.setindex!(m::AbstractOverseer, v::T, e::Entity) where {T<:ComponentData}
	entity_assert(m, e)
	ensure_component!(m, T)
	if !in(e, m[T])
        m[T][e] = v
        register_new!(m, T, e)
        return v
    end
	return m[T][e] = v
end

function register_new!(m::AbstractOverseer, ::Type{T}, e::Entity) where {T<:ComponentData}
    for g in groups(m)
        if !(g isa OrderedGroup)
            continue
        elseif T in g
            register_new!(find_lowest_child(g, T), e)
            return
        end
    end
end

function ensure_component!(m::AbstractOverseer, c::Type{<:ComponentData})
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

function Base.push!(m::AbstractOverseer, stage::SystemStage)
    comps = requested_components(stage)
    for c in comps
        ensure_component!(m, c)
    end
    push!(system_stages(m), stage)
    prepare(stage, m)
end

function Base.insert!(m::AbstractOverseer, i::Integer, stage::SystemStage)
    comps = requested_components(stage)
    for c in comps
        ensure_component!(m, c)
    end
    insert!(system_stages(m), i, stage)
    prepare(stage, m)
end

function Base.push!(m::AbstractOverseer, stage::Symbol, sys::System)
	stage = system_stage(m, stage) 
    comps = requested_components(sys)
    for c in comps
        ensure_component!(m, c)
    end
	push!(stage, sys)
    prepare(sys, m)
end

function Base.insert!(m::AbstractOverseer, stage::Symbol, i::Int, sys::System)
	insert!(system_stage(m, stage), i, sys)
    comps = requested_components(sys)
    for c in comps
        ensure_component!(m, c)
    end
    prepare(sys, m)
end

function Base.delete!(m::AbstractOverseer, e::Entity)
	entity_assert(m, e)
	push!(free_entities(m), e)
	entities(m)[e.id] = EMPTY_ENTITY
	for c in components(m)
		if in(e, c)
			pop!(c, e)
		end
	end
end

function empty_entities!(m::AbstractOverseer)
	empty!(entities(m))
	empty!(free_entities(m))
	for c in components(m)
    	empty!(c)
	end
end

function components(manager::AbstractOverseer, ::Type{T}) where {T<:ComponentData}
	comps = AbstractComponent[]
	for c in components(manager)
		if eltype(c) <: T
			push!(comps, c)
		end
	end
	return comps
end

function entity_assert(m::AbstractOverseer, e::Entity)
	es = entities(m)
	@assert length(es) >= e.id "$e was never initiated."
	@assert es[e.id] != EMPTY_ENTITY "$e was removed previously."
end

function schedule_delete!(m::AbstractOverseer, e::Entity)
	entity_assert(m, e)
	push!(to_delete(m), e)
end

function delete_scheduled!(m::AbstractOverseer)
	for c in components(m)
		delete!(c, to_delete(m))
	end
	for e in to_delete(m)
		entities(m)[e.id] = EMPTY_ENTITY
		push!(free_entities(m), e)
	end
end

function update(s::SystemStage, m::AbstractOverseer)
    for s in last(s)
        update(s, m)
    end
end

function update(m::AbstractOverseer)
	for stage in system_stages(m)
		update(stage, m)
	end
end

function prepare(m::AbstractOverseer)
	for s in system_stages(m)
		prepare(s, m)
	end
end
