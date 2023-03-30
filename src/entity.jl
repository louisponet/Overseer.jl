########################################
#                                      #
#            Entity                    #
#                                      #
########################################

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

function entity(c::AbstractComponent, i::Integer)
    return Entity(c.indices.packed[i])
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

Base.iterate(e::Entity, state=1) = state > 1 ? nothing : (e, state+1)

const EMPTY_ENTITY = Entity(0)

Base.:(==)(e1::AbstractEntity, e2::AbstractEntity) = e1.id == e2.id

########################################
#                                      #
#            EntityState               #
#                                      #
########################################

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
    ex = :(error("$(e.e) does not have a Component with field $f."))
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
