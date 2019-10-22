"Can be used to specify the type of component storage to be used for a given `ComponentData`."
component_type(::Type{<:ComponentData}) = Component

# Used together with the indexing scheme to put stubs in
# component arrays that lack the components for a particular index.
struct Stub <: ComponentData end

indices_iterator(a::AbstractComponent) = a.indices
"""
The most basic Component type.

Indexing into a component with an `Entity` will return the data linked to that entity,
indexing with a regular `Int` will return directly the `ComponentData` that is stored in the data
vector at that index, i.e. generally not the storage linked to the `Entity` with that `Int` as id.
"""
struct Component{T<:ComponentData} <: AbstractComponent{T}
    indices::Indices
    data::Vector{T}
end

Component{T}() where {T} = Component(Indices(), T[])


const EMPTY_COMPONENT = Component{Stub}()

"""
A shared component works very much like a normal component except that it tries to not have duplicate
data for different entities. This should be used for very large `ComponentData`. 
"""
struct SharedComponent{T<:ComponentData} <: AbstractComponent{T}
    indices::Indices
    data::Vector{Int} #saves the indices into the sharedfor each of the entities
    shared::Vector{T}
end

SharedComponent{T}() where {T<:ComponentData} = SharedComponent{T}(Indices(), Int[], T[])

##### BASE Extensions ####
Base.eltype(::Type{<:AbstractComponent{T}}) where T = T

Base.length(c::AbstractComponent) = length(c.data)

Base.in(i::Integer, c::AbstractComponent) = in(i, c.indices)
Base.in(e::Entity, c::AbstractComponent)  = in(e.id, c)

Base.isempty(c::AbstractComponent) = isempty(c.data)

function Base.delete!(c::AbstractComponent, es::Vector{Entity})
    for e in es
        if e in c
            pop!(c, e)
        end
    end
end

Base.@propagate_inbounds @inline Base.getindex(c::Component, e::Entity) = c.data[c.indices[e.id]]
Base.@propagate_inbounds @inline Base.getindex(c::SharedComponent, e::Entity) = c.shared[c.data[c.indices[e.id]]]
Base.@propagate_inbounds @inline Base.getindex(c::Component, i::Integer) = c.data[i]
Base.@propagate_inbounds @inline Base.getindex(c::SharedComponent, i::Integer) = c.shared[i]

@inline function Base.setindex!(c::Component{T}, v::T, e::Entity) where {T}
    eid = e.id
    @boundscheck if !in(e, c)
        push!(c.indices, eid)
        push!(c.data, v)
    else
        @inbounds c.data[c.indices[eid]] = v
    end
    return v
end
@inline function Base.setindex!(c::SharedComponent{T}, v::T, e::Entity) where {T}
    eid = e.id
    t_shared_id = findfirst(x -> x==v, c.shared)
    shared_id = t_shared_id === nothing ? (push!(c.shared, v); length(c.shared)) : t_shared_id
    @boundscheck if !in(e, c)
        push!(c.indices, eid)
        push!(c.data, shared_id)
    else
        @inbounds c.data[c.indices[eid]] = shared_id
    end
    return v
end

function Base.empty!(c::Component)
    empty!(c.indices)
    empty!(c.data)
    return c
end
function Base.empty!(c::SharedComponent)
    empty!(c.indices)
    empty!(c.data)
    empty!(c.shared)
    return c
end

function pop_indices_data!(c::AbstractComponent, e::Entity)
    @boundscheck if !in(e, c)
        throw(BoundsError(c, e))
    end
    n = length(c)
    @inbounds begin
        id = c.indices[e.id]
        v = c.data[id]
        c.data[id] = c.data[end]
        pop!(c.data)
        pop!(c.indices, e.id)
        return v 
    end
end

Base.pop!(c::Component, e::Entity) = pop_indices_data!(c, e)

function Base.pop!(c::SharedComponent, e::Entity)
    i = pop_indices_data!(c, e)
    idvec = c.data
    val = c.shared[i]
    if !any(isequal(i), idvec)
        for j in 1:length(idvec)
            if idvec[j] > i
                idvec[j] -= 1
            end
        end
        deleteat!(c.shared, i)
    end
    return val
end

@inline Base.iterate(c::Component, args...) = iterate(c.data, args...)
@inline Base.iterate(c::SharedComponent, args...) = iterate(c.shared, args...)

function ensure_entity_id!(c::AbstractComponent, e::Entity, id::Int)
    indices = c.indices
    @inbounds packed_id = indices[e.id]
    if packed_id != id
        set_packed_id!(indices, e.id, id)
        c.data[id], c.data[packed_id] = c.data[packed_id], c.data[id]
    end
end

struct Group
    component_ids::NTuple
    indices::Indices
    len::Int
end

function Group(cs)
    l, id = findmin(map(length, cs))
    shortest = cs[id]
    entity_dataid_map = Pair{Entity, Int}[]
    counter = 0
    for (i, e) in enumerate(EntityIterator(shortest.indices))
        if all(x -> in(e, x), cs)
            counter += 1
            push!(entity_dataid_map, e => counter)
        end
    end
    for (e, datid) in entity_dataid_map
        for c in cs
            ensure_entity_id!(c, e, datid)
        end
    end
    return Group(map(x -> component_id(eltype(x)), cs), cs[1].indices, counter)
end

Base.length(g::Group) = g.len
@inline indices_iterator(g::Group) = g
@inline indices(g::Group) = g.indices

function Base.iterate(g::Group, state=1)
    if state > length(g)
        return nothing
    end
    return @inbounds g.indices.packed[state], state+1
end

@inline Base.in(c::Int, g::Group) = c ∈ g.component_ids

########################################
#                                      #
#            Iteration                 #
#                                      #
########################################
struct EntityIterator{T<:Union{IndicesIterator,Indices,Group}}
    it::T
end

Base.length(i::EntityIterator) = length(i.it)

@inline function Base.iterate(i::EntityIterator, state=1)
    n = iterate(i.it, state)
    n === nothing && return n
    return Entity(n[1]), n[2]
end

macro entities_in(indices_expr)
    expr, t_sets, t_orsets = expand_indices_bool(indices_expr)
    if length(t_sets) == 1 && isempty(t_orsets)
        return esc(:(ECS.EntityIterator(ECS.indices_iterator($(t_sets[1])))))
    else
        return esc(quote
            t_comps = $(Expr(:tuple, t_sets...))
            t_or_comps = $(Expr(:tuple, t_orsets...))
            sets = map(ECS.indices_iterator, t_comps)
            orsets = map(ECS.indices_iterator, t_or_comps)
            if isempty(sets)
                minlen, minid = findmin(map(length, orsets))
                t_shortest = orsets[minid]
            else
                minlen, minid = findmin(map(length, sets))
                t_shortest = sets[minid]
            end
            if $(!isempty(t_orsets))
                shortest = deepcopy(t_shortest)
                for s in orsets
                    union!(shortest, s)
                end
            else
                shortest = t_shortest
            end
            ECS.EntityIterator(ECS.IndicesIterator(shortest, x -> $expr, length(shortest)))
        end)
    end
end

########################################
#                                      #
#     ComponentData indexing scheme    #
#                                      #
########################################

const COMPONENTDATA_TYPES = Symbol[]

component_id(::Type{<:ComponentData}) = -1

function typename(typedef::Expr)
    if typedef.args[2] isa Symbol
        return typedef.args[2]
    elseif typedef.args[2].args[1] isa Symbol
        return typedef.args[2].args[1]
    elseif typedef.args[2].args[1].args[1] isa Symbol
        return typedef.args[2].args[1].args[1]
    else
        error("Could not parse type-head from: $typedef")
    end
end

function process_typedef(typedef, mod, with_kw=false)
	tn = ECS.typename(typedef)
	ctypes = COMPONENTDATA_TYPES
	if !(tn in ctypes)
    	push!(ctypes, tn)
        if typedef.args[2] isa Symbol
        	typedef.args[2] = Expr(Symbol("<:"), tn, ECS.ComponentData)
        elseif typedef.args[2].head == Symbol("<:")
            if !Base.eval(mod, :($(typedef.args[2].args[2]) <: ECS.ComponentData))
                error("Components can only have supertypes which are subtypes of ComponentData.")
            end
    	else
        	error("Components can not have type parameters")
        	# typ_pars = typedef.args[2].args[2:end]
        	# typedef.args[2] = Expr(Symbol("<:"), Expr(:curly, tn, typ_pars...), :ComponentData)
    	end
    	id = length(COMPONENTDATA_TYPES)
        if with_kw
            tq = quote
            	ECS.Parameters.@with_kw $typedef
            	ECS.component_id(::Type{$tn}) = $id
            end
        else
            tq = quote
            	$typedef
            	ECS.component_id(::Type{$tn}) = $id
            end
        end
    	return tq, tn
    end
end

macro component(typedef)
	return esc(ECS._component(typedef, __module__))
end
macro component_with_kw(typedef)
	return esc(ECS._component(typedef, __module__, true))
end

function _component(typedef, mod::Module, args...)
    t = process_typedef(typedef, mod, args...)
    if t !== nothing
    	t1, tn = t 
    	return quote
    	    $t1
        	ECS.component_type(::Type{$tn}) = ECS.Component
    	end
	end
end

macro shared_component(typedef)
	return esc(ECS._shared_component(typedef, __module__))
end

macro shared_component_with_kw(typedef)
	return esc(ECS._shared_component(typedef, __module__, true))
end

function _shared_component(typedef, mod::Module, args...)
    t = process_typedef(typedef, mod, args...)
    if t !== nothing
    	t1, tn = t 
    	return quote
    	    $t1
        	ECS.component_type(::Type{$tn}) = ECS.SharedComponent
    	end
	end
end

