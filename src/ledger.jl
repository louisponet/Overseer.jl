"""
    Ledger

A `Ledger` holds all the [`Entities`](@ref Entity), [`Components`](@ref) and [`Systems`](@ref). It has interfaces to create new [`Entities`](@ref Entity) and access the [`Components`](ref). Calling [`update(ledger)`](@ref Overseer.update) will call
all the [`update`](@ref) functions of the systems in the `Ledger`.
"""
mutable struct Ledger <: AbstractLedger
    entities     ::Vector{Entity}
    free_entities::Vector{Entity}
    to_delete    ::Vector{Entity}
    components   ::Dict{DataType, AbstractComponent}
    groups       ::Vector{AbstractGroup}

    stages::Vector{Stage}
end
Ledger() = Ledger(Entity[],
                    Entity[],
                    Entity[],
                    Dict{DataType, AbstractComponent}(),
                    AbstractGroup[],
                    Stage[])

function Ledger(comps::Dict{DataType, AbstractComponent})
    out = Ledger()
    out.components = comps
    out.entities = Entity.(union(map(x->x.indices.packed, values(comps))...))
    return out 
end

function Ledger(cs::AbstractComponent...)
    comps = Dict{DataType, AbstractComponent}()
    for c in cs
        comps[eltype(c)] = c
    end
    return Ledger(comps)
end

Ledger(components::Type...) = Ledger(map(x -> component_type(x)(), components)...)

function Ledger(stages::Stage...)
    m = Ledger()
    m.stages=[stages...]
    prepare(m)
    return m
end

ledger(m::Ledger) = m

components(m::AbstractLedger)       = ledger(m).components
entities(m::AbstractLedger)         = ledger(m).entities
free_entities(m::AbstractLedger)    = ledger(m).free_entities
to_delete(m::AbstractLedger)        = ledger(m).to_delete
stage(m::AbstractLedger, name)      = stage(ledger(m), name)
valid_entities(m::AbstractLedger)   = filter(x -> x.id != 0, entities(m))
stages(m::AbstractLedger)           = ledger(m).stages
groups(m::AbstractLedger)           = ledger(m).groups
singleton(m::AbstractLedger, ::Type{T}) where {T} = EntityState(Entity(entity(m[T], 1)), m[T])

##### BASE Extensions ####
function Base.show(io::IO, ::MIME"text/plain", l::AbstractLedger)
    summary(io, l)
    println(io)
    println(io, "Components:")
    compstrings = []
    for (k, c) in components(l)
        push!(compstrings, split(summary(c)))
    end
    for s in sort(compstrings, by = x->x[2])
        print(io, "  ")
        println(io, join(s, " "))
    end
    println(io, "Total entities: $(length(entities(l)) - length(free_entities(l)))")
end
function Base.in(::Type{R}, m::AbstractLedger) where {R}
    return R ∈ keys(components(m))
end

function Base.empty!(m::AbstractLedger)
    empty!(entities(m))
    empty!(free_entities(m))
    empty!(to_delete(m))
    empty!(components(m))
    empty!(stages(m))
    empty!(groups(m))
end

function Base.getindex(m::AbstractLedger, ::Type{T}) where {T}
    return get!(components(m), T, component_type(T)())
end

Base.copy(m::AbstractLedger) = Ledger(copy(entities(m)),
                                      copy(free_entities(m)),
                                      copy(to_delete(m)),
                                      deepcopy(components(m)),
                                      deepcopy(groups(m)), 
                                      deepcopy(stages(m)))

function Base.getindex(m::AbstractLedger, e::AbstractEntity)
    entity_assert(m, e)        
    data = AbstractComponent[]
    for c in values(components(m))
        if in(e, c)
            push!(data, c)
        end
    end
    return EntityState(convert(Entity, e), (data...,))
end

function stage(l::Ledger, s::Symbol)
    id = findfirst(x-> x.name == s, l.stages)
    if id === nothing
        error("Stage $s not found.")
    end
    return l.stages[id]
end

function Base.setindex!(m::AbstractLedger, v::T, e::AbstractEntity) where {T}
    entity_assert(m, e)
    ensure_component!(m, T)
    if !in(e, m[T])
        m[T][e] = v
        register_new!(m, T, e)
        return v
    end
    return m[T][e] = v
end

function Base.setindex!(m::AbstractLedger, v::C, ::Type{T}) where {T, C <: AbstractComponent{T}}
    return components(m)[T] = v
end

function register_new!(m::AbstractLedger, ::Type{T}, e::AbstractEntity) where {T}
    for g in groups(m)
        if !(g isa OrderedGroup)
            continue
        elseif T in g
            register_new!(find_lowest_child(g, T), e)
            return
        end
    end
end

function ensure_component!(m::AbstractLedger, c::Type{T}) where {T}
    if !(c in m)
        components(m)[T] = component_type(c)()
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

function Base.delete!(m::AbstractLedger, e::AbstractEntity)
    entity_assert(m, e)
    push!(free_entities(m), e)
    entities(m)[e.id] = EMPTY_ENTITY
    for c in values(components(m))
        if in(e, c)
            pop!(c, e)
        end
    end
end

Base.isequal(F::C, G::C) where {C <: AbstractLedger} =
    all(f -> isequal(getfield(F, f), getfield(G, f)), 1:nfields(F))::Bool
    
Base.:(==)(F::C, G::C) where {C <: AbstractLedger} =
    all(f -> getfield(F, f)== getfield(G, f), 1:nfields(F))::Bool
    
@inline function Base.hash(l::AbstractLedger, h::UInt)
    for i in nfields(l)
        h = hash(getfield(l, i), h)
    end
    return h
end

function empty_entities!(m::AbstractLedger)
    empty!(entities(m))
    empty!(free_entities(m))
    for c in values(components(m))
        empty!(c)
    end
end

function components(ledger::AbstractLedger, ::Type{T}) where {T}
    comps = AbstractComponent[]
    for c in values(components(ledger))
        if eltype(c) <: T
            push!(comps, c)
        end
    end
    return comps
end

function Base.in(e::AbstractEntity, m::AbstractLedger)
    es = entities(m)
    return length(es) >= Entity(e).id && es[Entity(e).id] != EMPTY_ENTITY
end

function entity_assert(m::AbstractLedger, e::AbstractEntity)
    @assert e in m "$(Entity(e)) does not exist, either never initiated or removed previously."
end

function schedule_delete!(m::AbstractLedger, e::Entity)
    entity_assert(m, e)
    push!(to_delete(m), e)
end

function schedule_delete!(m::AbstractLedger, e::EntityState)
    entity_assert(m, e)
    push!(to_delete(m), e.e)
end

function delete_scheduled!(m::AbstractLedger)
    for c in values(components(m))
        delete!(c, to_delete(m))
    end
    for e in to_delete(m)
        entities(m)[e.id] = EMPTY_ENTITY
        push!(free_entities(m), e)
    end
    empty!(to_delete(m))
end

function update(m::AbstractLedger)
    for stage in stages(m)
        update(stage, m)
    end
end

"""
    prepare(m)

Goes through all [`Stages`](@ref Stage) in `m`, makes sure that all [`requested_components`](@ref) are present in `m`,
and then calls [`prepare`](@ref) on the [`Stage`](@ref).
"""
function prepare(m::AbstractLedger)
    for stage in stages(m)
        for ct in requested_components(stage)
            ensure_component!(m, ct)
        end
        prepare(stage, m)
    end
end
