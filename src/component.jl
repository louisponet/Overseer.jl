using Test

struct TestCompData
    t::Int
end

# AbstractComponent Interface
function test_abstractcomponent_interface(::Type{T}) where {T<:AbstractComponent}
    c = T{TestCompData}()

    @test eltype(c) <: TestCompData
    @test c.indices isa Indices
    
    @test isempty(c)
    @test length(c) == 0
    c[Entity(1)] = TestCompData(1)
    c[Entity(2)] = TestCompData(1)
    @test Entity(2) in c
    @test length(c) == 2 == size(c)[1] == length(c.indices) == length(entity_data(c))

    
    @test c[Entity(2)] isa TestCompData

    @test entity(c, 1) isa Entity
    @test pop!(c, Entity(2)) == TestCompData(1)
    @test pop!(c) == EntityState(Entity(1), TestCompData(1))
    @test isempty(c)
    
    c[Entity(1)] = TestCompData(1)
    c[Entity(2)] = TestCompData(2)

    # Needed for order swapping
    entity_data(c)[1], entity_data(c)[2] = entity_data(c)[2], entity_data(c)[1]
    @test c[Entity(2)] == TestCompData(1)
    @test c[Entity(1)] == TestCompData(2)
    
    c[Entity(1)], c[Entity(2)] = c[Entity(2)], c[Entity(1)]
    @test c[Entity(1)] == TestCompData(1)
    @test c[Entity(2)] == TestCompData(2)
    
    @test iterate(c)[1] isa TestCompData
    empty!(c)
    @test isempty(c)
end

"""
    component_type(::Type) 

Function that can be overloaded to specify what the default [`AbstractComponent`](@ref) of a given type is. This is mainly used in the various component macros.
"""
component_type(::Type{TC}) where{TC} = Component{TC}
 
@inline indices_iterator(a::AbstractComponent) = a.indices
@inline reverse_indices_iterator(a::AbstractComponent) = ReverseIndicesIterator(a.indices, i -> true)

Base.in(i::Integer,        c::AbstractComponent)  = in(i, c.indices)
Base.in(e::AbstractEntity, c::AbstractComponent)  = in(e.id, c)

Base.length(c::AbstractComponent)  = length(c.indices)
Base.size(c::AbstractComponent)    = size(c.indices)
Base.isempty(c::AbstractComponent) = isempty(c.indices)

Base.eltype(::Type{<:AbstractComponent{T}}) where T = T

function Base.delete!(c::AbstractComponent, es::Vector{Entity})
    for e in es
        if e in c
            pop!(c, e)
        end
    end
end

Base.@propagate_inbounds function Base.getindex(c::AbstractComponent, I::AbstractVector{<:AbstractEntity})
    return map(x -> c[x], I)
end

"""
    swap_order!(c, entity1, entity2)

Swaps the order of the data of `entity1` and `entity2`. This is useful to order
multiple components in the same way so that iteration can be performed without
performing checks, i.e. when using [`Groups`](@ref Groups).
"""
function swap_order!(c::AbstractComponent, e1::AbstractEntity, e2::AbstractEntity)
    @boundscheck if !in(e1, c)
        throw(BoundsError(c, Entity(e1)))
    elseif !in(e2, c)
        throw(BoundsError(c, Entity(e2)))
    end
    @inbounds begin
        id1, id2 = swap_order!(c.indices, e1.id, e2.id)
        edata = entity_data(c)
        edata[id1], edata[id2] = edata[id2], edata[id1]
    end
end

function Base.permute!(c::AbstractComponent, permvec::AbstractVector{<:Integer})
    permute!(entity_data(c), permvec)
    permute!(c.indices, permvec)
    return c
end

Base.permute!(c::AbstractComponent, permvec::AbstractVector{<:AbstractEntity}) = permute!(c, map(x->c.indices[x.id], permvec))

Base.sortperm(c::AbstractComponent, args...; kwargs...) = sortperm(entity_data(c), args...; kwargs...)

function Base.:(==)(c1::C1, c2::C2) where {C1 <: AbstractComponent, C2 <: AbstractComponent}
    if eltype(C1) != eltype(C2) ||length(c1) != length(c2)
        return false
    elseif length(c1) > 20 && hash(c1) != hash(c2)
        return false
    else
        return all(i -> (e = entity(c1, i); (e in c2) && (@inbounds c2[e] == c1[e])), eachindex(c1))
    end
end

Base.IndexStyle(::Type{AbstractComponent}) = IndexLinear()


"""
The most basic Component type.

Indexing into a component with an [`Entity`](@ref) will return the data linked to that entity,
indexing with a regular `Int` will return directly the data that is stored in the data
vector at that index, i.e. generally not the storage linked to the [`Entity`](@ref) with that `Int` as id.
"""
mutable struct Component{T} <: AbstractComponent{T}
    indices::Indices
    data   ::Vector{T}
end

Component{T}() where {T} = Component(Indices(), T[])

entity_data(c::Component) = c.data

##### BASE Extensions ####
Base.@propagate_inbounds @inline Base.getindex(c::Component, i::Integer) = c.data[i]

@inline function Base.getindex(c::Component, e::AbstractEntity)
    eid = e.id
    @boundscheck if !in(e, c)
        throw(BoundsError(c, Entity(e)))
    end
    return @inbounds c.data[c.indices[eid]]
end

@inline function Base.setindex!(c::Component{T}, v::T, e::AbstractEntity) where {T}
    eid = e.id
    @boundscheck if !in(e, c)
        push!(c.indices, eid)
        push!(c.data, v)
        return v
    end
    @inbounds c.data[c.indices[eid]] = v
    return v
end

function Base.empty!(c::Component)
    empty!(c.indices)
    empty!(c.data)
    return c
end

function Base.pop!(c::Component, e::AbstractEntity)
    @boundscheck if !in(e, c)
        throw(BoundsError(c, Entity(e)))
    end
    @inbounds begin
        id = c.indices[e.id]
        v = c.data[id]
        c.data[id] = c.data[end]
        pop!(c.data)
        pop!(c.indices, e.id)
        return v 
    end
end

function Base.pop!(c::AbstractComponent)
    @boundscheck if isempty(c)
        throw(BoundsError(c))
    end
    @inbounds begin
        return EntityState(Entity(pop!(c.indices)), pop!(c.data))
    end
end

@inline Base.iterate(c::Component, args...) = iterate(c.data, args...)

@inline function Base.hash(c::C, h::UInt) where {C <: AbstractComponent}
    for f in nfields(c)
        h = hash(getfield(c,f), h)
    end
    return h
end

##############################
#                            #
#     Component Macros       #
#                            #
##############################
function process_typedef(typedef, mod)
    global td = nothing
    MacroTools.postwalk(typedef) do x
        if @capture(x, struct T_ fields__ end | mutable struct T_ fields__ end)
            global td = T
        end
        x
    end
    return MacroTools.namify(td)
end

"""
    @component

This takes a struct definition and 
"""
macro component(typedef)
	return esc(Overseer._component(typedef, __module__))
end
function _component(typedef, mod)
    tn = process_typedef(typedef, mod)
    return quote
        Base.@__doc__($typedef)
        Overseer.component_type(::Type{T}) where {T<:$tn} = Overseer.Component{T}
    end
end

########################################
#                                      #
#            PooledComponent           #
#                                      #
########################################

struct ApplyToPool
    e::Entity
end

Base.parent(e::Entity) = ApplyToPool(e)

mutable struct PooledComponent{T} <: AbstractComponent{T}
    indices::Indices
    pool::Vector{Int}
    pool_size::Vector{Int}
    data::Vector{T}
end

PooledComponent{T}() where {T} = PooledComponent{T}(Indices(), Int[], Int[], T[])
Base.@propagate_inbounds @inline pool(c::PooledComponent, e::AbstractEntity) = c.pool[c.indices[e.id]]
Base.@propagate_inbounds @inline pool(c::PooledComponent, e::Int) = c.pool[c.indices[e]]

entity_data(c::PooledComponent) = c.pool

npools(c::PooledComponent) = length(c.data)

Base.@propagate_inbounds @inline Base.getindex(c::PooledComponent, i::Integer) = c.data[c.pool[i]]

@inline function Base.getindex(c::PooledComponent, e::AbstractEntity)
    @boundscheck if !in(e, c)
        throw(BoundsError(c, Entity(e)))
    end
    return @inbounds c.data[pool(c, e)]
end

Base.@propagate_inbounds @inline function Base.parent(c::PooledComponent, i::Int)
    @boundscheck if i > length(c.data)
        throw(BoundsError(c, i))
    end
    return @inbounds Entity(c.indices.packed[findfirst(isequal(i), c.pool)])
end
Base.@propagate_inbounds @inline Base.parent(c::PooledComponent, e::Entity) = parent(c, pool(c, e))
    

# c[entity] = value
# set value of <only> this entity
@inline function Base.setindex!(c::PooledComponent{T}, v::T, e::AbstractEntity) where {T}
    eid = e.id
    @inbounds if in(e, c)
        pid = c.indices[eid]
        g = c.pool[pid]
        if c.pool_size[g] == 1
            # the entity already has its own (otherwise empty) pool - adjust value
            c.data[g] = v
        else
            # the entity is part of a larger pool - create a new one
            c.pool_size[g] -= 1
            push!(c.data, v)
            push!(c.pool_size, 1)
            c.pool[pid] = length(c.data)
        end
    else
        # the entity is not in the component - add it
        push!(c.indices, eid)
        push!(c.pool, length(c.data)+1)
        push!(c.pool_size, 1)
        push!(c.data, v)
    end
    return v
end

# c[entity] = parent
# set the value of this entity to that of parent
@inline function Base.setindex!(c::PooledComponent, p::AbstractEntity, e::AbstractEntity)
    @boundscheck if !in(p, c)
        throw(BoundsError(c, Entity(p)))
    end
    @inbounds begin
        pg = pool(c, p)
        if in(e, c)
            eg = pool(c, e)
            if c.pool_size[eg] == 1
                # if this entity is the only one holding onto a value, remove 
                # that value and cleanup pool indices
                deleteat!(c.data, eg)
                deleteat!(c.pool_size, eg)
                for i in eachindex(c.pool)
                    c.pool[i] = c.pool[i] - (c.pool[i] > eg)
                end
            else
                c.pool_size[eg] -= 1
            end
            # adjust pool index either way
            c.pool[c.indices[e.id]] = pg
        else
            # if the entity is not in there we have to add it
            push!(c.indices, e.id)
            push!(c.pool, pg)
        end
        c.pool_size[pg] += 1

        return c[p]
    end
end

# c[ParentGroup(entity)] = value
# set the value for all entities pooled with entity
@inline function Base.setindex!(c::PooledComponent{T}, v::T, x::ApplyToPool) where {T}
    e = x.e
    @boundscheck if !in(e, c)
        throw(BoundsError(c, Entity(e)))
    end
    @inbounds c.data[pool(c, e.id)] = v
    return v
end

function Base.empty!(c::PooledComponent)
    empty!(c.indices)
    empty!(c.pool)
    empty!(c.pool_size)
    empty!(c.data)
    return c
end

function maybe_cleanup_empty_pool!(c::PooledComponent, poolid)
    if c.pool_size[poolid] == 0
        deleteat!(c.data, poolid)
        deleteat!(c.pool_size, poolid)
        for i in eachindex(c.pool)
            if c.pool[i] > poolid
                c.pool[i] -= 1
            end
        end
    end
end

function Base.pop!(c::PooledComponent, e::AbstractEntity)
    @boundscheck if !in(e, c)
        throw(BoundsError(c, Entity(e)))
    end

    @inbounds begin
        id = c.indices[e.id]
        g = c.pool[id]

        c.pool[id] = c.pool[end]
        c.pool_size[g] -= 1
        pop!(c.pool)
        pop!(c.indices, e.id)

        val = c.data[g]
        maybe_cleanup_empty_pool!(c, g)

        return val
    end

end
function Base.pop!(c::PooledComponent)
    @boundscheck if isempty(c)
        throw(BoundsError(c))
    end
    @inbounds begin
        e = Entity(pop!(c.indices))
        g = pop!(c.pool)
        val = c.data[g]
        c.pool_size[g] -= 1
        maybe_cleanup_empty_pool!(c, g)
        return EntityState(e, val)
    end
end

@inline function Base.iterate(c::PooledComponent, args...)
    n = iterate(c.pool, args...)
    n === nothing && return n
    return @inbounds c.data[n[1]], @inbounds n[2]
end

Base.sortperm(c::PooledComponent) = sortperm(c.pool)

macro pooled_component(typedef)
    return esc(Overseer._pooled_component(typedef, __module__))
end

function _pooled_component(typedef, mod)
    tn = process_typedef(typedef, mod) 
    return quote
        Base.@__doc__($typedef)
        Overseer.component_type(::Type{T}) where {T<:$tn} = Overseer.PooledComponent{T}
    end
end

function make_unique!(c::PooledComponent)
    @inbounds begin
        # Go through all pools and check if a later pool has the same value.
        # If it does mark the later pool empty.
        for i in eachindex(c.pool)
            g0 = c.pool[i]
            if c.pool_size[g0] > 0
                v0 = c.data[g0]
                for j in i+1:length(c.pool)
                    g = c.pool[j]
                    if c.pool_size[g] > 0 && c.data[g] == v0
                        c.pool_size[g] -= 1
                        c.pool_size[g0] += 1
                        c.pool[j] = g0
                    end
                end
            end
        end

        # Go through all pools again and replace empty pools (i.e. its data 
        # and pool_size) by the current last pool. Adjust pool indices 
        # accordingly.
        i = 1
        while i <= length(c.pool_size)
            if c.pool_size[i] == 0
                if i == length(c.pool_size)
                    pop!(c.pool_size)
                    pop!(c.data)
                    break
                else
                    N = length(c.pool_size)
                    c.pool_size[i] = pop!(c.pool_size)
                    c.data[i] = pop!(c.data)
                    for j in eachindex(c.pool)
                        c.pool[j] = c.pool[j] == N ? i : c.pool[j]
                    end
                end
            else
                i += 1
            end
        end
    end

    return
end

