@inline function Base.:(==)(c1::C, c2::C) where {C<:ComponentData} 
    for f in fieldnames(C) 
        if !(getfield(c1, f) == getfield(c2, f)) 
            return false 
        end 
    end 
    return true 
end

"Can be used to specify the type of component storage to be used for a given `ComponentData`."
component_type(::Type{<:ComponentData}) = Component
 
# Used together with the indexing scheme to put stubs in
# component arrays that lack the components for a particular index.
struct Stub <: ComponentData end

@inline indices_iterator(a::AbstractComponent) = a.indices

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

function Base.permute!(c::AbstractComponent, permvec)
    permute!(c.data, permvec)
    permute!(c.indices, permvec)
end

Base.@propagate_inbounds @inline Base.getindex(c::Component, e::Entity) = c.data[c.indices[e.id]]
Base.@propagate_inbounds @inline Base.getindex(c::SharedComponent, e::Entity) = c.shared[c.data[c.indices[e.id]]]
Base.@propagate_inbounds @inline Base.getindex(c::Component, i::Integer) = c.data[i]
Base.@propagate_inbounds @inline Base.getindex(c::SharedComponent, i::Integer) = c.shared[c.data[i]]

@inline function Base.setindex!(c::Component{T}, v::T, e::Entity) where {T}
    eid = e.id
    @boundscheck if !in(e, c)
        push!(c.indices, eid)
        push!(c.data, v)
        return v
    end
    @inbounds c.data[c.indices[eid]] = v
    return v
end
@inline function Base.setindex!(c::SharedComponent{T}, v::T, e::Entity) where {T}
    eid = e.id
    t_shared_id = findfirst(x -> x==v, c.shared)
    shared_id = t_shared_id === nothing ? (push!(c.shared, v); length(c.shared)) : t_shared_id
    @boundscheck if !in(e, c)
        push!(c.indices, eid)
        push!(c.data, shared_id)
        return v
    end
    @inbounds c.data[c.indices[eid]] = shared_id
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

function ensure_entity_id!(c::AbstractComponent, e::Int, id::Int)
    indices = c.indices
    @inbounds packed_id = indices[e]
    if packed_id != id
        set_packed_id!(indices, e, id)
        c.data[id], c.data[packed_id] = c.data[packed_id], c.data[id]
    end
    return true
end

function shared_entity_ids(cs)
    l, id = findmin(map(length, cs))
    shortest = cs[id]
    shared_entity_ids = Int[]
    for (i, e) in enumerate(shortest.indices)
        if all(x -> in(e, x.indices), cs)
            push!(shared_entity_ids, e)
        end
    end
    return shared_entity_ids
end

Base.sortperm(c::SharedComponent) = sortperm(c.data)

########################################
#                                      #
#            Iteration                 #
#                                      #
########################################
struct EntityIterator{T<:Union{IndicesIterator,Indices,AbstractGroup}}
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
    if length(t_sets) == 1 && isempty(t_orsets) && expr.args[2] isa Symbol
        return esc(:(Overseer.EntityIterator(Overseer.indices_iterator($(t_sets[1])))))
    else
        return esc(quote
            t_comps = $(Expr(:tuple, t_sets...))
            t_or_comps = $(Expr(:tuple, t_orsets...))
            sets = map(Overseer.indices_iterator, t_comps)
            orsets = map(Overseer.indices_iterator, t_or_comps)
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
            Overseer.EntityIterator(Overseer.IndicesIterator(shortest, x -> $expr, length(shortest)))
        end)
    end
end

Base.getindex(iterator::EntityIterator, i) = Entity(iterator.it.shortest.packed[i])

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
    if !isdefined(mod, :COMPONENTDATA_TYPES)
        Base.eval(mod, :(const COMPONENTDATA_TYPES = Symbol[]))
    end
	tn = Overseer.typename(typedef)
	ctypes = mod.COMPONENTDATA_TYPES
	if !(tn in ctypes)
    	push!(ctypes, tn)
        if typedef.args[2] isa Symbol
        	typedef.args[2] = Expr(Symbol("<:"), tn, Overseer.ComponentData)
        elseif typedef.args[2].head == Symbol("<:")
            if !Base.eval(mod, :($(typedef.args[2].args[2]) <: Overseer.ComponentData))
                error("Components can only have supertypes which are subtypes of ComponentData.")
            end
    	else
        	error("Components can not have type parameters")
        	# typ_pars = typedef.args[2].args[2:end]
        	# typedef.args[2] = Expr(Symbol("<:"), Expr(:curly, tn, typ_pars...), :ComponentData)
    	end
    	id = length(mod.COMPONENTDATA_TYPES)
        if with_kw
            tq = quote
            	Overseer.Parameters.@with_kw $typedef
            	Overseer.component_id(::Type{$tn}) = $id
            end
        else
            tq = quote
            	$typedef
            	Overseer.component_id(::Type{$tn}) = $id
            end
        end
    	return tq, tn
    end
end

macro component(typedef)
	return esc(Overseer._component(typedef, __module__))
end
macro component_with_kw(typedef)
	return esc(Overseer._component(typedef, __module__, true))
end

function _component(typedef, mod::Module, args...)
    t = process_typedef(typedef, mod, args...)
    if t !== nothing
    	t1, tn = t 
    	return quote
    	    $t1
        	Overseer.component_type(::Type{$tn}) = Overseer.Component
    	end
	end
end

macro shared_component(typedef)
	return esc(Overseer._shared_component(typedef, __module__))
end

macro shared_component_with_kw(typedef)
	return esc(Overseer._shared_component(typedef, __module__, true))
end

function _shared_component(typedef, mod::Module, args...)
    t = process_typedef(typedef, mod, args...)
    if t !== nothing
    	t1, tn = t 
    	return quote
    	    $t1
        	Overseer.component_type(::Type{$tn}) = Overseer.SharedComponent
    	end
	end
end

