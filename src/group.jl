"Forms groups of components." 

@inline indices(g::AbstractGroup) = g.indices

function Base.iterate(g::AbstractGroup, state=1)
    if state > length(g)
        return nothing
    end
    return @inbounds g.indices.packed[state], state+1
end

@inline indices_iterator(g::AbstractGroup) = g

"Groups components, creating a vector of the shared entity ids."
struct BasicGroup <: AbstractGroup
    component_ids::NTuple
    indices::Indices
end

BasicGroup(cs) = BasicGroup(map(x -> component_id(eltype(x)), cs), Indices(shared_entity_ids(cs)))

@inline Base.in(c::Int, g::BasicGroup) = c ∈ g.indices
Base.length(bg::BasicGroup) = length(bg.indices)








"Groups components, and makes sure that the order of entities is the same in each of the components."
mutable struct OrderedGroup{CT,N} <: AbstractGroup
    components::CT
    component_ids::NTuple{N, Int}
    indices::Indices
    len::Int
    parent::Union{Nothing, OrderedGroup}
    child::Union{Nothing, OrderedGroup}
end

@inline Base.in(c::Entity, g::OrderedGroup) = c.id ∈ g.indices && g.indices[c.id] <= length(g)
@inline Base.in(::Type{T}, g::OrderedGroup) where {T} = component_id(T) ∈ g.component_ids

function OrderedGroup(cs, parent, child)
    valid_entities = shared_entity_ids(cs)
    for (datid, e) in enumerate(valid_entities)
        for c in cs
            ensure_entity_id!(c, e, datid)
        end
    end
    return OrderedGroup(cs, map(x -> component_id(eltype(x)), cs), cs[1].indices, length(valid_entities), parent, child)
end

Base.length(g::OrderedGroup) = g.len

Base.in(c::AbstractComponent, g::OrderedGroup) = c in g.components

group(m::AbstractManager,  cs::Type{<:ComponentData}...; ordered=true) = group(m, map(x -> m[x], cs))

function group(m::AbstractManager, comps)
    for g in groups(m)
        if !(g isa OrderedGroup)
            continue
        end
        t_g = group(g, comps)
        if t_g !== nothing
            return t_g
        end
    end

    parent_group = findfirst(g -> all(in(g), comps), groups(m))

    if parent_group !== nothing
        parent = find_lowest_parent(groups(m)[parent_group], comps)
        if length(parent.components) == length(comps) || (parent.child !== nothing && length(parent.child.components) == length(comps))
            throw(ArgumentError("An ordered group with at least one but not all of the components $(eltype.(comps)) already exists.\nAn ordered component group can only be a subgroup of a previously ordered group."))
        end

        child  = deepcopy(parent.child)
        g = OrderedGroup(comps, parent, child)
        parent.child = g
        return g
    else
        child_group = findfirst(g -> all(in(comps), g.components) && g isa OrderedGroup, groups(m))
        if child_group === nothing
            any_group = findfirst(g -> any(in(g.components), comps), groups(m))
            if any_group !== nothing

                throw(ArgumentError("An ordered group with at least one but not all of the components $(eltype.(comps)) already exists.\nAn ordered component group can only be a subgroup of a previously ordered group."))
            end
            g = OrderedGroup(comps, nothing, nothing)
            push!(groups(m), g)
        else
            g = OrderedGroup(comps, nothing, groups(m)[child_group])
            g.child.parent = g
            groups(m)[child_group] = g
        end
        return g
    end
end

function find_lowest_parent(g::OrderedGroup, cs)
    if length(g.components) < length(cs)
        return g.parent
    elseif g.child === nothing
        return g
    else
        return find_lowest_parent(g.child, cs)
    end
end

function group(g::OrderedGroup, cs)
    if all(in(g), cs) && length(g.components) == length(cs)
        return g
    else
        return group(g.child, cs)
    end
end

group(::Nothing, cs) = nothing

@generated function Base.getindex(g::OrderedGroup{CT}, ::Type{T}) where {CT,T<:ComponentData}
    tid = findfirst(x -> eltype(x) == T, CT.parameters)
    return :(g.components[$tid])
end

Base.@propagate_inbounds @inline function Base.getindex(g::OrderedGroup, e::Entity)
    id = g.indices[e.id]
    return map(x -> x[id], g.components)
end

@inline function Base.setindex!(g::OrderedGroup, v::T, e::Entity) where {T<:ComponentData}
    comp = g[T]
    @boundscheck if !(e in comp)
        if g.child !== nothing && T in g.child
            setindex!(g.child, v, e)
        else
            comp[e] = v
        end
        g.len += 1
        map(x -> ensure_entity_id!(x, e.id, g.len), g.components)
        return v
    end
    return @inbounds comp[e] = v
end

@inline function regroup!(m::AbstractManager, cs::Type{<:ComponentData}...)
    gid = group_id(m, cs)
    gt = typeof(groups(m)[gid])
    groups(m)[gid] = gt(map(x->m[x], cs))
end

@inline function regroup!(m::AbstractManager)
    for (i, g) in enumerate(groups(m))
        groups(m)[i] = typeof(g)(map(x->components(m)[x], g.component_ids)) 
    end
    return groups(m)
end

function remove_group!(m::AbstractManager, cs::Type{<:ComponentData}...)
    ids = findall(x -> all(in(x), cs) && length(x.component_ids) == length(cs), groups(m))
    if ids !== nothing
        deleteat!(groups(m), ids)
    end
end
