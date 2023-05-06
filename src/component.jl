using Test

struct TestCompData
    t::Int
end

# AbstractComponent Interface
"""
Tests whether an [`AbstractComponent`](@ref) satisfies the interface.
"""
function test_abstractcomponent_interface(::Type{T}) where {T<:AbstractComponent}
    c = T{TestCompData}()

    @test eltype(c) <: TestCompData
    if hasfield(T, :indices)
        @test c.indices isa Indices
    else
        @test indices_iterator(c) isa IndicesIterator
        @test reverse_indices_iterator(c) isa ReverseIndicesIterator
    end

    @test isempty(c)
    @test length(c) == 0
    c[Entity(1)] = TestCompData(1)
    c[Entity(2)] = TestCompData(1)
    @test Entity(2) in c
    @test length(c) == 2 == size(c)[1] == length(c.indices) == length(data(c))

    @test c[Entity(2)] isa TestCompData

    @test entity(c, 1) isa EntityState{Tuple{Base.RefArray{TestCompData, T{TestCompData}, Nothing}}}
    @test pop!(c, Entity(2)) == TestCompData(1)
    @test pop!(c) == EntityState(Entity(1), (Ref(TestCompData(1)),))
    @test isempty(c)

    c[Entity(1)] = TestCompData(1)
    c[Entity(2)] = TestCompData(2)

    # Needed for order swapping
    data(c)[1], data(c)[2] = data(c)[2], data(c)[1]
    @test c[Entity(2)] == TestCompData(1)
    @test c[Entity(1)] == TestCompData(2)

    c[Entity(1)], c[Entity(2)] = c[Entity(2)], c[Entity(1)]
    @test c[Entity(1)] == TestCompData(1)
    @test c[Entity(2)] == TestCompData(2)

    @test iterate(c)[1] isa TestCompData
    empty!(c)
    @test isempty(c)
end

## AbstractComponent Interface: For new components that have a standard component underneath, just overload `component` to point to it

component(c::AbstractComponent) = MethodError(component, c)

@inline data(c::AbstractComponent) = data(component(c))
@inline indices(c::AbstractComponent) = indices(component(c))
@inline data_index(c::AbstractComponent, args...) = data_index(component(c), args...)

@inline Base.Ref(c::AbstractComponent, e::AbstractEntity) = Ref(c, data_index(c, e))

"""
    in(entity, component)

Checks whether `entity` has a data entry in `component`.
"""
Base.in(i::Integer, c::AbstractComponent) = in(i, indices(c))
Base.in(e::AbstractEntity, c::AbstractComponent) = in(Entity(e).id, c)

Base.length(c::AbstractComponent)  = length(indices(c))
Base.size(c::AbstractComponent)    = size(indices(c))
Base.isempty(c::AbstractComponent) = isempty(indices(c))

Base.eltype(::Type{<:AbstractComponent{T}}) where {T} = T

Base.delete!(c::AbstractComponent, es::Vector{Entity}) =
    for e in es
        if e in c
            pop!(c, e)
        end
    end

Base.pop!(c::AbstractComponent, args...) = pop!(component(c), args...)

Base.empty!(c::AbstractComponent) = empty!(component(c))


Base.@propagate_inbounds @inline Base.getindex(c::AbstractComponent, i::Integer) = data(c)[data_index(c, i)]

@inline function Base.getindex(c::AbstractComponent, e::AbstractEntity)
    @boundscheck if !in(e, c)
        throw(BoundsError(c, Entity(e)))
    end
    return @inbounds data(c)[data_index(c, e)]
end

Base.@propagate_inbounds function Base.getindex(c::AbstractComponent,
                                                I::AbstractVector{<:AbstractEntity})
    return map(x -> c[x], I)
end

Base.@propagate_inbounds @inline Base.setindex!(c::AbstractComponent, v, i::Integer) = (setindex!(data(c), v, data_index(c, i)); return c)
Base.@propagate_inbounds @inline Base.setindex!(c::AbstractComponent, v, e::AbstractEntity) =
    (setindex!(component(c), v, e); return c)

function Base.permute!(c::AbstractComponent, permvec::AbstractVector{<:Integer})
    permute!(data(c), permvec)
    return c
end

function Base.permute!(c::AbstractComponent, permvec::AbstractVector{<:AbstractEntity})
    return permute!(c, map(x -> indices(c)[x.id], permvec))
end

function Base.sortperm(c::AbstractComponent, args...; kwargs...)
    return sortperm(data(c), args...; kwargs...)
end

function Base.:(==)(c1::C1, c2::C2) where {C1<:AbstractComponent,C2<:AbstractComponent}
    if eltype(C1) != eltype(C2) || length(c1) != length(c2)
        return false
    elseif length(c1) > 20 && hash(c1) != hash(c2)
        return false
    else
        return all(i -> (e = entity(c1, i); (e in c2) && (@inbounds c2[e] == c1[e])),
                   eachindex(c1))
    end
end

Base.IndexStyle(::Type{AbstractComponent}) = IndexLinear()

Base.iterate(c::AbstractComponent, args...) = iterate(component(c), args...)

"""
    component_type(::Type) 

Function that can be overloaded to specify what the default [`AbstractComponent`](@ref) of a given type is. This is mainly used in the various component macros.
"""
component_type(::Type{TC}) where {TC} = Component{TC}

@inline indices_iterator(a::AbstractComponent)::Indices = indices(a)

@inline function reverse_indices_iterator(a::AbstractComponent)
    return ReverseIndicesIterator(indices(a), i -> true)
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
        id1, id2 = swap_order!(indices(c), e1.id, e2.id)
        edata = data(c)
        edata[id1], edata[id2] = edata[id2], edata[id1]
    end
end

"""
The most basic Component type.

Indexing into a component with an [`Entity`](@ref) will return the data linked to that entity,
indexing with a regular `Int` will return directly the data that is stored in the data
vector at that index, i.e. generally not the storage linked to the [`Entity`](@ref) with that `Int` as id.

To register a `struct` to be stored in a [`Component`](@ref) see [`@component`](@ref).
"""
mutable struct Component{T} <: AbstractComponent{T}
    indices::Indices
    data::Vector{T}
end

Component{T}() where {T} = Component(Indices(), T[])

@inline indices(c::Component) = c.indices
@inline data(c::Component) = c.data

Base.@propagate_inbounds @inline data_index(c::Component, i::Integer) = i
Base.@propagate_inbounds @inline data_index(c::Component, e::AbstractEntity) = c.indices[e.id]

@inline component(c::Component) = c

##### BASE Extensions ####

@inline function Base.setindex!(c::Component{T}, v::T, e::AbstractEntity) where {T}
    eid = Entity(e).id
    @boundscheck if !in(e, c)
        push!(c.indices, eid)
        push!(c.data, v)
        return c
    end
    
    setindex!(c.data, v, c.indices[eid])
    
    return c
end

function Base.empty!(c::Component)
    empty!(c.indices)
    empty!(c.data)
    return c
end

"""
    pop!(component, entity)

pops the data for `entity` out of `component`.
"""
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
        return EntityState(Entity(pop!(c.indices)), (Ref(pop!(c.data)),))
    end
end

@inline Base.iterate(c::Component, args...) = iterate(c.data, args...)

@inline function Base.hash(c::C, h::UInt) where {C<:AbstractComponent}
    for f in nfields(c)
        h = hash(getfield(c, f), h)
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
This takes a struct definition and register it so that it will be stored inside a [`Component`](@ref) when attached to [Entities](@ref).

# Example
```julia
@component struct MyComp
    v::Float64
end
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

"""
A [`PooledComponent`](@ref) allows for sharing data between many [Entities](@ref).
Essentially, the indices into the data pool are stored for each [`Entity`](@ref), rather than the data itself.

To make a `struct` to be stored inside [`PooledComponent`](@ref) see [`@pooled_component`](@ref).

[`make_unique!`](@ref) can be used to check whether duplicate data exists and if so point all [Entities](@ref) to only
a single copy and remove the duplicates.

To interact with the data pools see [`pool`](@ref), [`pools`](@ref) and [`entity_pool`](@ref).

# Example
```julia
@pooled_component struct Comp1
    comp1
end

e1 = Entity(ledger, Comp1(1))
e2 = Entity(ledger)

ledger[Comp1][e2] = e1 # The Comp1 data for e2 is set to point to the same as e1 
```
"""
mutable struct PooledComponent{T} <: AbstractComponent{T}
    indices::Indices
    pool::Vector{Int}
    pool_size::Vector{Int}
    data::Vector{T}
end

PooledComponent{T}() where {T} = PooledComponent{T}(Indices(), Int[], Int[], T[])

@inline component(c::PooledComponent) = c
@inline indices(c::PooledComponent) = c.indices
@inline data(c::PooledComponent) = c.data

"""
    pool(pooled_compnent, entity)
    pool(pooled_compnent, i)
    
Returns which pool `entity` or the `ith` entity belongs to.
"""
Base.@propagate_inbounds @inline function pool(c::PooledComponent, e::AbstractEntity)
    return c.pool[c.indices[e.id]]
end
Base.@propagate_inbounds @inline pool(c::PooledComponent, e::Int) = c.pool[c.indices[e]]


Base.@propagate_inbounds @inline data_index(c::PooledComponent, i::Integer) = c.pool[i] 
Base.@propagate_inbounds @inline data_index(c::PooledComponent, e::AbstractEntity) = pool(c, e)


npools(c::PooledComponent) = length(c.data)

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
    if in(e, c)
        @inbounds begin
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
        end
    else
        # the entity is not in the component - add it
        push!(c.indices, eid)
        push!(c.pool, length(c.data) + 1)
        push!(c.pool_size, 1)
        push!(c.data, v)
    end
    return c
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

        return c
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
    return c
end

function Base.empty!(c::PooledComponent)
    empty!(c.indices)
    empty!(c.pool)
    empty!(c.pool_size)
    empty!(c.data)
    return c
end

function maybe_cleanup_empty_pool!(c::PooledComponent, poolid)
    c.pool_size[poolid] != 0 && return

    deleteat!(c.data, poolid)
    deleteat!(c.pool_size, poolid)

    for i in eachindex(c.pool)
        if c.pool[i] > poolid
            c.pool[i] -= 1
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
        return EntityState(e, (Ref(val),))
    end
end

@inline function Base.iterate(c::PooledComponent, args...)
    n = iterate(c.pool, args...)
    n === nothing && return n
    return @inbounds c.data[n[1]], @inbounds n[2]
end

Base.sortperm(c::PooledComponent) = sortperm(c.pool)

"""
This takes a struct definition and register it so that it will be stored inside a [`PooledComponent`](@ref) when attached to [Entities](@ref).
"""
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

"""
Checks whether duplicate data exists in a [`PooledComponent`](@ref) and if so points all [Entities](@ref) to only
a single copy while removing the duplicates.
"""
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

function Base.permute!(c::PooledComponent, permvec::AbstractVector{<:Integer})
    permute!(c.pool, permvec)
    return c
end

function swap_order!(c::PooledComponent, e1::AbstractEntity, e2::AbstractEntity)
    @boundscheck if !in(e1, c)
        throw(BoundsError(c, Entity(e1)))
    elseif !in(e2, c)
        throw(BoundsError(c, Entity(e2)))
    end
    @inbounds begin
        id1, id2 = swap_order!(indices(c), e1.id, e2.id)
        edata = c.pool
        edata[id1], edata[id2] = edata[id2], edata[id1]
    end
end
