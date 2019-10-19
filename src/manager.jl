#To implement AbstractManager interface, the subtype should just define the function manager
#where all the fields are like in this one

"""
    Basic Manager. This takes care of creating and destroying entities,
    making sure all the requested components for the systems are initialized,
    and updates the systems.
"""
mutable struct Manager <: AbstractManager
	entities     ::Vector{Entity}
	free_entities::Vector{Entity}
	to_delete    ::Vector{Entity}
	components   ::Vector{Union{Component,SharedComponent}}
	# components   ::Dict{DataType, Union{Component,SharedComponent}}

	system_stages::Vector{SystemStage}
end
Manager() = Manager(Entity[],
                    Entity[],
                    Entity[],
                    Union{Component,SharedComponent}[],
                    Pair{Symbol, Vector{System}}[])

function Manager(comps::Vector{Union{Component, SharedComponent}})
    out = Manager()
    out.components = comps
    return out 
end

function Manager(cs::AbstractComponent...)
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
	return Manager(comps)
end

# Manager(cs::AbstractComponent...) = Manager(Entity[], Entity[], Dict([eltype(x) => x for x in cs]), System[])
Manager(components::Type{<:ComponentData}...) = Manager(map(x -> component_type(x){x}(), components)...)

function Manager(system_stages::SystemStage...)
	comps = Type{<:ComponentData}[] 
	for stage in system_stages
		append!(comps, requested_components(stage)) 
	end
	m = Manager(comps...)
	m.system_stages=[system_stages...]
	return m
end

manager(m::Manager)                                               = m

components(m::AbstractManager)                                    = manager(m).components
entities(m::AbstractManager)                                      = manager(m).entities
free_entities(m::AbstractManager)                                 = manager(m).free_entities
to_delete(m::AbstractManager)                                     = manager(m).to_delete
valid_entities(m::AbstractManager)                                = filter(x -> x.id != 0, entities(m))
system_stages(m::AbstractManager)                                 = manager(m).system_stages
system_stage(m::AbstractManager, s::Symbol)                       = manager(m).system_stages[s]
singleton(m::AbstractManager, ::Type{T}) where {T<:ComponentData} = m[T][1]


##### BASE Extensions ####
function Base.in(::Type{R}, m::AbstractManager) where {R<:ComponentData}
    cid = component_id(R)
    return cid <= length(components(m)) && components(m)[cid] !== EMPTY_COMPONENT
end

function Base.empty!(m::AbstractManager)
	empty!(entities(m))
	empty!(free_entities(m))
	empty!(components(m))
	empty!(system_stages(m))
end

function Base.getindex(m::AbstractManager, ::Type{T}) where {T<:ComponentData}
	id = component_id(T)
	return components(m)[id]::component_type(T){T}
end
function Base.getindex(m::AbstractManager, e::Entity)
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

function Base.setindex!(m::AbstractManager, v::T, e::Entity) where {T<:ComponentData}
	entity_assert(m, e)
	if !in(T, m)
		append!(m, [T])
	end
	return m[T][e] = v
end

function Base.setindex!(m::AbstractManager, v, ::Type{T}, es::Vector{Entity}) where {T<:ComponentData}
	comp = m[T]
	for e in es
		entity_assert(m, e)
		comp[e] = v
	end
	return v
end

function Base.append!(m::AbstractManager, comps)
    m_comps = components(m)
    for c in comps
        id = component_id(c)
        while id > length(m_comps)
            push!(m_comps, EMPTY_COMPONENT)
        end
        if !(c in m)
            comp = component_type(c){c}()
            m_comps[id] = comp
        end
    end
    return m_comps
end

function Base.push!(m::AbstractManager, stage::SystemStage)
    comps = requested_components(stage)
    append!(m, comps)
    push!(system_stages(m), stage)
end

function Base.insert!(m::AbstractManager, i::Integer, stage::SystemStage)
    comps = requested_components(stage)
    append!(m, comps)
    insert!(system_stages(m), i, stage)
end

function Base.push!(m::AbstractManager, stage::Symbol, sys::System)
	stage = system_stage(m, stage) 
    comps = requested_components(sys)
    append!(m, comps)
	push!(stage, sys)
end

function Base.insert!(m::AbstractManager, stage::Symbol, i::Int, sys::System)
	insert!(system_stage(m, stage), i, sys)
    comps = requested_components(sys)
    append!(m, comps)
end

function Base.delete!(m::AbstractManager, e::Entity)
	entity_assert(m, e)
	push!(free_entities(m), e)
	entities(m)[e.id] = EMPTY_ENTITY
	for c in components(m)
		if in(e, c)
			pop!(c, e)
		end
	end
end

function empty_entities!(m::AbstractManager)
	empty!(entities(m))
	empty!(free_entities(m))
	for c in components(m)
    	empty!(c)
	end
end

function components(manager::AbstractManager, ::Type{T}) where {T<:ComponentData}
	comps = AbstractComponent[]
	for c in components(manager)
		if eltype(c) <: T
			push!(comps, c)
		end
	end
	return comps
end

function entity_assert(m::AbstractManager, e::Entity)
	es = entities(m)
	@assert length(es) >= e.id "$e was never initiated."
	@assert es[e.id] != EMPTY_ENTITY "$e was removed previously."
end

function schedule_delete!(m::AbstractManager, e::Entity)
	entity_assert(m, e)
	push!(to_delete(m), e)
end

function delete_scheduled!(m::AbstractManager)
	for c in components(m)
		delete!(c, to_delete(m))
	end
	for e in to_delete(m)
		entities(m)[e.id] = EMPTY_ENTITY
		push!(free_entities(m), e)
	end
end

function update_systems(s::SystemStage, m::AbstractManager)
    for s in last(s)
        update(s, m)
    end
end

function update_systems(m::AbstractManager)
	for stage in system_stages(m)
		update_systems(stage, m)
	end
end

update_stage(m::AbstractManager, s::Symbol) = 
	update_systems(system_stage(m, s), m)

function prepare(m::AbstractManager)
	for s in system_stages(m)
		prepare(s, m)
	end
end
