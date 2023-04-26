########################################
#                                      #
#            Entity                    #
#                                      #
########################################

"""
    Entity

Can be thought of as simply an index to retrieve data associated with the [`Entity`](@ref) from [Components](@ref) or a [`Ledger`](@ref).
In the case of a [`Ledger`](@ref) it will return an [`EntityState`](@ref) that holds references to all the [Components](@ref) for which the
[`Entity`](@ref) has data.

An [`Entity`](@ref) should always first be created from an [`AbstractLedger`](@ref) by using `Entity(ledger, comps...)` before it is used. 

# Example
```julia
e = Entity(ledger, Comp1(), Comp2())
ledger[e] # EntityState with references to easily retrieve e's data for Comp1 and Comp2

ledger[Comp2][e] # access the Comp2 data of e
ledger[e][Comp2] # identical to above
```
"""
struct Entity <: AbstractEntity
    id::Int
end

function Entity(m::AbstractLedger)
    if !isempty(free_entities(m))
        e = pop!(free_entities(m))
        entities(m)[e.id] = e
        return e
    end
    n = length(entities(m)) + 1
    e = Entity(n)
    push!(entities(m), e)
    return e
end

function Entity(m::AbstractLedger, datas...)
    e = Entity(m)
    for d in datas
        m[e] = d
    end
    return e
end

function Entity(m::AbstractLedger, parent::Entity, datas...)
    e = Entity(m)
    for d in datas
        m[e] = d
    end
    for (T, component) in components(m)
        if component isa PooledComponent && in(parent, component) && !in(e, component)
            component[e] = parent
        end
    end
    return e
end

Entity(e::Entity) = e

Base.@propagate_inbounds Entity(c::AbstractComponent, i::Int) = Entity(c.indices.packed[i])

Base.iterate(e::Entity, state=1) = state > 1 ? nothing : (e, state+1)

const EMPTY_ENTITY = Entity(0)

Base.:(==)(e1::AbstractEntity, e2::AbstractEntity) = e1.id == e2.id

########################################
#                                      #
#            EntityState               #
#                                      #
########################################

"""
    EntityState

Combination of an [`Entity`](@ref) and a bunch of [Components](@ref) for which the [`Entity`](@ref) has data.
It can thus be used as an index similar to a standard [`Entity`](@ref), but it also provides some nice ways to
retrieve the data associated with it in the [Components](@ref).

# Example

```julia
@component struct Comp1
    comp1
end

@component mutable struct Comp2
    comp2
end

@component struct Comp3
    comp3
end    


for e in @entities_in(ledger, Comp1 && Comp2)
    # e is an EntityState
    # we can directly access the fields of Comp1 and Comp2 since they have unique names,
    # and assign a Comp3 to the Entity that's represented by the EntityState
    
    ledger[Comp3][e] = Comp3(e.comp1 + e.comp2)
    # If there are components that have the same name in the EntityState you can access them directly
    e[Comp1].comp1
    e[Comp2].comp2

    # If the Components are mutable we can also set their fields using
    e.comp2 = 3
end
```
"""
struct EntityState{TT<:Tuple} <: AbstractEntity
    e::Entity
    components::TT
end
EntityState(e::Entity, comps...) = EntityState(e, comps)

Entity(e::EntityState) = e.e
Base.convert(::Type{Entity}, e::EntityState) = Entity(e)
function Base.show(io::IO, e::EntityState)
    println(io, "$(typeof(e)):")
    println(io, "$(e.e)")
    println(io, "Components:")
    for c in e.components
        if c isa AbstractComponent
            println(io, "$(c[e])")
        else
            println(io, "$c")
        end
    end
end
components(e::EntityState) = e.components
Base.in(::Type{T}, e::EntityState{TT}) where {T, TT} = any(x->eltype(x) == T, TT.parameters)

# TODO: Cleanup, can these two be merged?
@generated function Base.getproperty(e::EntityState{TT}, f::Symbol) where {TT}
    fn_to_DT = Dict{Symbol, DataType}()
    ex = :(getfield(e, f))
    ex = Expr(:elseif, :(f === :id), :(return getfield(getfield(e, :e), :id)::Int), ex)
    for PDT in TT.parameters
        DT = PDT <: AbstractComponent ? eltype(PDT) : PDT
        
        for (fn, ft) in zip(fieldnames(DT), fieldtypes(DT))
            if haskey(fn_to_DT, fn)
                fnq = QuoteNode(fn)
                DT_ = fn_to_DT[fn]
                ft_ = fieldtype(DT_, fn)
                ex = MacroTools.postwalk(ex) do x
                    if @capture(x, return getfield(e[$DT_], $fnq)::$ft_)
                        return quote
                            error("Field $f found in multiple components in $e.\nPlease use entity_state[$($DT)].$f instead.")
                        end
                    else
                        return x
                    end
                end
            else
                fn_to_DT[fn] = DT
                fnq = QuoteNode(fn)
                ex = Expr(:elseif, :(f === $fnq), :(return getfield(e[$DT], $fnq)::$ft), ex)
            end
        end
    end
    ex.head = :if
    return quote
        $(Expr(:meta, :inline))
        $(Expr(:meta, :propagate_inbounds))
        $ex
    end
end

@generated function Base.setproperty!(e::EntityState{TT}, f::Symbol, val) where {TT}
    fn_to_DT = Dict{Symbol, DataType}()
    ex = :(error("$e does not have a Component with field $f."))
    for PDT in TT.parameters
        DT = PDT <: AbstractComponent ? eltype(PDT) : PDT
        for (fn, ft) in zip(fieldnames(DT), fieldtypes(DT))
            if haskey(fn_to_DT, fn)
                fnq = QuoteNode(fn)
                DT_ = fn_to_DT[fn]
                ft_ = fieldtype(DT_, fn)
                ex = MacroTools.postwalk(ex) do x
                    if @capture(x, setfield!(e[$DT_], $fnq, val))
                        return quote
                            error("Field $f found in multiple components in $e.\nPlease use <entitystate>[$($DT)].$f instead.")
                        end
                    else
                        return x
                    end
                end
            else
                fn_to_DT[fn] = DT
                fnq = QuoteNode(fn)
                ex = Expr(:elseif, :(f === $fnq), :(return setfield!(e[$DT], $fnq, val)), ex)
            end
        end
    end
    ex.head = :if
    return quote
        $(Expr(:meta, :inline))
        $(Expr(:meta, :propagate_inbounds))
        $ex
    end
end

@generated function component(e::EntityState{TT}, ::Type{T}) where {TT<:Tuple, T}
    id = findfirst(x -> x <: AbstractComponent ? eltype(x) == T : x == T, TT.parameters)
    if id === nothing
        error("EntityState{$TT} has no Component{$T}.")
    end
    return quote
        $(Expr(:meta, :inline))
        @inbounds getfield(e,:components)[$id]
    end
end


@inline function Base.getindex(e::EntityState, ::Type{T}) where {T}
    t = component(e, T)
    if t isa AbstractComponent
        return @inbounds t[e.e]
    else
        return t
    end
end
    
@inline function Base.setindex!(e::EntityState, x::T, ::Type{T}) where {T}
    t = component(e, T)
    @assert t isa AbstractComponent "Cannot set a Component in a non referenced EntityState."
    return @inbounds t[e] = x
end
    
@inline Base.length(::EntityState{TT}) where {TT} = length(TT.parameters)
@inline function Base.iterate(i::EntityState, state = 1)
    state > length(i) && return nothing
    return @inbounds i.components[state][i.e], state + 1
end
@inline Base.@propagate_inbounds Base.getindex(e::EntityState, i::Int) = e.components[i][e.e]

"""
    entity(c, i)

Return the [`EntityState`](@ref) with the `i`th [`Entity`](@ref) and data in [Component](@ref Components) `c`.
"""
function entity(c::AbstractComponent, i::Integer)
    return EntityState(Entity(c, i), c)
end

"""
    last_entity(c::AbstractComponent)

Retrieves the last [`Entity`](@ref) in `c`.
"""
last_entity(c::AbstractComponent) = entity(c, length(c))

