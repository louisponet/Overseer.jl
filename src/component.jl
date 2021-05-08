Base.isequal(F::C, G::C) where {C <: ComponentData} =
    all(f -> isequal(getfield(F, f), getfield(G, f)), 1:nfields(F))::Bool
    
Base.:(==)(F::C, G::C) where {C <: ComponentData} =
    all(f -> getfield(F, f)== getfield(G, f), 1:nfields(F))::Bool

@inline function Base.hash(c::C, h::UInt) where {C <: ComponentData}
    for f in nfields(c)
        h = hash(getfield(c, f), h)
    end
    return h
end

"Can be used to specify the type of component storage to be used for a given `ComponentData`."
component_type(::Type{<:ComponentData}) = Component
 
@inline indices_iterator(a::AbstractComponent) = a.indices

"""
The most basic Component type.

Indexing into a component with an `Entity` will return the data linked to that entity,
indexing with a regular `Int` will return directly the `ComponentData` that is stored in the data
vector at that index, i.e. generally not the storage linked to the `Entity` with that `Int` as id.
"""
struct Component{T <: ComponentData} <: AbstractComponent{T}
    indices::Indices
    data   ::Vector{T}
end

Component{T}() where {T} = Component(Indices(), T[])


"""
A shared component works very much like a normal component except that it tries to not have duplicate
data for different entities. This should be used for very large `ComponentData`. 
"""
struct SharedComponent{T <: ComponentData} <: AbstractComponent{T}
    indices::Indices
    data   ::Vector{Int} # saves the indices into the shared for each of the entities
    shared ::Vector{T}
end

SharedComponent{T}() where {T <: ComponentData} = SharedComponent{T}(Indices(), Int[], T[])

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

Base.@propagate_inbounds @inline Base.getindex(c::Component,       e::AbstractEntity)  = c.data[c.indices[e.id]]
Base.@propagate_inbounds @inline Base.getindex(c::SharedComponent, e::AbstractEntity)  = c.shared[c.data[c.indices[e.id]]]
Base.@propagate_inbounds @inline Base.getindex(c::Component,       i::Integer) = c.data[i]
Base.@propagate_inbounds @inline Base.getindex(c::SharedComponent, i::Integer) = c.shared[c.data[i]]

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
@inline function Base.setindex!(c::SharedComponent{T}, v::T, e::AbstractEntity) where {T}
    eid = e.id
    t_shared_id = findfirst(x->x == v, c.shared)
    shared_id = t_shared_id === nothing ? (push!(c.shared, v); length(c.shared)) : t_shared_id
    @boundscheck if !in(e, c)
        push!(c.indices, eid)
        push!(c.data, shared_id)
        return v
    end
    @inbounds c.data[c.indices[eid]] = shared_id
    return v
end

#TODO This is necessary  to support || in @entities_in, do we need that?
# See below for the code without
@inline function Base.pointer(c::Component{T}, e::AbstractEntity) where {T}
    if e.id in c.indices
        if T.mutable
            unsafe_load(reinterpret(Ptr{Ptr{T}}, pointer(c.data, @inbounds c.indices[e.id])))
        else
            return pointer(c.data, @inbounds c.indices[e.id])
        end
    else
        return Ptr{T}()
    end
end
        
@inline function Base.pointer(c::SharedComponent{T}, e::AbstractEntity) where {T}
    if e.id in c.indices
        if T.mutable
            unsafe_load(reinterpret(Ptr{Ptr{T}}, pointer(c.shared, @inbounds c.data[c.indices[e.id]])))
        else
            return pointer(c.shared, @inbounds c.data[c.indices[e.id]])
        end
    else
        return Ptr{T}()
    end
end
# @inline Base.pointer(c::Component{T}, e::Entity) where {T}=
#     pointer(c.data, @inbounds c.indices[e.id])
# @inline Base.pointer(c::SharedComponent{T}, e::Entity) where {T} =
#     pointer(c.shared, @inbounds c.data[c.indices[e.id]])

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

function swap_order!(c::AbstractComponent, e1::AbstractEntity, e2::AbstractEntity)
    @boundscheck if !in(e1, c)
        throw(BoundsError(c, e1))
    elseif !in(e2, c)
        throw(BoundsError(c, e2))
    end
    @inbounds begin
        id1, id2 = swap_order!(c.indices, e1.id, e2.id)
        c.data[id1], c.data[id2] = c.data[id2], c.data[id1]
    end
end

function pop_indices_data!(c::AbstractComponent, e::AbstractEntity)
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

Base.pop!(c::Component, e::AbstractEntity) = pop_indices_data!(c, e)

function Base.pop!(c::SharedComponent, e::AbstractEntity)
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
        @inbounds id_to_swap = indices.packed[id]
        swap_order!(indices, e, id_to_swap)
        c.data[id], c.data[packed_id] = c.data[packed_id], c.data[id]
    end
    return true
end

function shared_entity_ids(cs)
    l, id = findmin(map(length, cs))
    shortest = cs[id]
    shared_entity_ids = Int[]
    for (i, e) in enumerate(shortest.indices)
        if all(x->in(e, x.indices), cs)
            push!(shared_entity_ids, e)
        end
    end
    return shared_entity_ids
end

Base.sortperm(c::SharedComponent) = sortperm(c.data)

@inline function Base.hash(c::C, h::UInt) where {C <: AbstractComponent}
    for f in nfields(c)
        h = hash(getfield(c,f), h)
    end
    return h
end

########################################
#                                      #
#            Iteration                 #
#                                      #
########################################
struct EntityIterator{T <: Union{IndicesIterator,Indices,AbstractGroup}, TT <: Tuple}
    it::T
    components::TT
end

@generated function Base.eltype(::EntityIterator{T, TT}) where {T, TT}
    t = map(x->Ptr{eltype(x)}, TT.parameters)
    return :(EntityState{Tuple{$t...}})
end
    
Base.IteratorSize(i::EntityIterator) = Base.IteratorSize(i.it)
Base.length(i::EntityIterator) = length(i.it)

struct EntityState{TT<:Tuple} <: AbstractEntity
    e::Entity
    pointers::TT
end

# TODO: Cleanup, can these two be merged?
@generated function Base.getproperty(e::EntityState{TT}, f::Symbol) where {TT}
    fn_to_DT = Dict{Symbol, DataType}()
    ex = :(getfield(e, f))
    ex = Expr(:elseif, :(f === :id), :(return getfield(getfield(e, :e), :id)::Int), ex)
    for PDT in TT.parameters
        DT = eltype(PDT)
        for (fn, ft) in zip(fieldnames(DT), fieldtypes(DT))
            if haskey(fn_to_DT, fn)
                fnq = QuoteNode(fn)
                DT_ = fn_to_DT[fn]
                ft_ = fieldtype(DT_, fn)
                ex = MacroTools.postwalk(ex) do x
                    if @capture(x, return getfield(e[$DT_], $fnq)::$ft_)
                        return quote
                            throw(error("Field $f found in multiple components in $e.\nPlease use entity_state[$($DT)].$f instead."))
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
    return ex
end

@generated function Base.setproperty!(e::EntityState{TT}, f::Symbol, val) where {TT}
    fn_to_DT = Dict{Symbol, DataType}()
    ex = :(getfield(e, f))
    ex = Expr(:elseif, :(f === :id), :(return setfield!(getfield(e, :e), :id)::Int, val), ex)
    for PDT in TT.parameters
        DT = eltype(PDT)
        for (fn, ft) in zip(fieldnames(DT), fieldtypes(DT))
            if haskey(fn_to_DT, fn)
                fnq = QuoteNode(fn)
                DT_ = fn_to_DT[fn]
                ft_ = fieldtype(DT_, fn)
                ex = MacroTools.postwalk(ex) do x
                    if @capture(x, setfield!(t, $fnq, val))
                        return quote
                            throw(error("Field $f found in multiple components in $e.\nPlease use entity_state[$($DT)].$f instead."))
                        end
                    else
                        return x
                    end
                end
            else
                fn_to_DT[fn] = DT
                fnq = QuoteNode(fn)
                tex = quote
                    ptr = valid_pointer(e, $DT)
                    t = Base.unsafe_pointer_to_objref(ptr)
                    setfield!(t, $fnq, val)
                end
                    
                ex = Expr(:elseif, :(f === $fnq), tex, ex)
            end
        end
    end
    ex.head = :if
    return ex
end

@generated function Base.pointer(e::EntityState{TT}, ::Type{T}) where {TT<:Tuple, T<:ComponentData}
    id = findfirst(x -> eltype(x) == T, TT.parameters)
    return :(getfield(e, :pointers)[$id])
end

#TODO: These boundchecks etc are to ensure in the case of || in @entities_in nobody is trying to load some
#      invalid memory. Do we really need ||?
#      diff in benchmarks is 47.8 vs 48.2 mus
#      Solution, have Ptr and MaybePtr so that the boundscheck can be done at compile time,
#      or have a different iterator for || vs &&

@inline function valid_pointer(e::EntityState, ::Type{T}) where {T<:ComponentData}
    ptr = pointer(e, T)
    if ptr === Ptr{T}()
        throw(error("$e does not have ComponentData $T"))
    end
    return ptr
end

Base.@propagate_inbounds @inline pointers(x::Tuple{}, e::Entity) = ()
Base.@propagate_inbounds @inline pointers(x::Tuple, e::Entity) = (pointer(first(x), e), pointers(Base.tail(x), e)...)

Base.getindex(e::EntityState, ::Type{T}) where {T<:ComponentData} =
        T.mutable ? Base.unsafe_pointer_to_objref(valid_pointer(e, T))::T : unsafe_load(valid_pointer(e, T))::T

Base.setindex!(e::EntityState, x::T, ::Type{T}) where {T<:ComponentData} =
    unsafe_store!(valid_pointer(e, T), x)

@generated function Base.in(e::EntityState{TT}, ::AbstractComponent{T}) where {T,TT}
    id = findfirst(x -> eltype(x) == T, TT.parameters)
    if id === nothing
        return :(false)
    else
        return :(getfield(e, :pointers)[$id] != Ptr{T}())
    end
end
   
@inline function Base.iterate(i::EntityIterator, state = 1)
    n = iterate(i.it, state)
    n === nothing && return n
    e = Entity(n[1])
    return EntityState(e, @inbounds pointers(i.components, e)), n[2]
end

Base.getindex(iterator::EntityIterator, i) = Entity(iterator.it.shortest.packed[i])
    
macro entities_in(indices_expr)
    expr, t_sets, t_orsets = expand_indices_bool(indices_expr)
    if length(t_sets) == 1 && isempty(t_orsets) && expr.args[2] isa Symbol
        return esc(:(Overseer.EntityIterator(Overseer.indices_iterator($(t_sets[1])), ($(t_sets[1]),))))
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
            Overseer.EntityIterator(Overseer.IndicesIterator(shortest, x->$expr), (t_comps..., t_or_comps...,))
        end)
    end
end

macro entities_in(ledger, indices_expr)
    expr, t_sets, t_orsets = expand_indices_bool(indices_expr)
    t_comp_defs = quote
    end
    comp_sym_map = Dict()
    for s in [t_sets; t_orsets]
        sym = gensym()
        t_comp_defs = quote
            $t_comp_defs
            $sym = $ledger[$s]
        end
        comp_sym_map[s] = sym
    end
    t_comp_defs = MacroTools.rmlines(MacroTools.flatten(t_comp_defs))
    
    expr = MacroTools.postwalk(expr) do x
        if x in keys(comp_sym_map)
            return comp_sym_map[x]
        else
            return x
        end
    end
    t_sets = map(x -> comp_sym_map[x], t_sets)
    t_orsets = map(x -> comp_sym_map[x], t_orsets)
    
    if length(t_sets) == 1 && isempty(t_orsets) && expr.args[2] isa Symbol
        return esc(quote
            $t_comp_defs
            Overseer.EntityIterator(Overseer.indices_iterator($(t_sets[1])), ($(t_sets[1]),))
        end)
    else
        return esc(quote
            $t_comp_defs
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
            Overseer.EntityIterator(Overseer.IndicesIterator(shortest, x->$expr), (t_comps..., t_or_comps...,))
        end)
    end
end    
    


function Base.:(==)(c1::C1, c2::C2) where {C1 <: AbstractComponent, C2 <: AbstractComponent}
    if eltype(C1) != eltype(C2) ||length(c1) != length(c2)
        return false
    elseif length(c1) > 20 && hash(c1) != hash(c2)
        return false
    else
        return all(e -> (e in c2) && (@inbounds c2[e] == c1[e]), @entities_in(c1))
    end
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
    tn = MacroTools.namify(td)
    if @capture(td, T_ <: V_)
        if !Base.eval(mod, :($V <: Overseer.ComponentData)) 
            error("Components can only have supertypes which are subtypes of ComponentData.")
        else
            return typedef, tn
        end
    else
        typedef_ = MacroTools.postwalk(typedef) do x
            if MacroTools.isexpr(x) && x.head == :struct
                x.args[2] = :($(x.args[2]) <: Overseer.ComponentData)
            end
            x
        end
        return typedef_, tn
    end
end

macro component(typedef)
	return esc(Overseer._component(typedef, __module__))
end
function _component(typedef, mod)
    t = process_typedef(typedef, mod)
	t1, tn = t
	return quote
	    $t1
        Overseer.component_type(::Type{$tn}) = Overseer.Component
    end
end

macro shared_component(typedef)
    return esc(Overseer._shared_component(typedef, __module__))
end

function _shared_component(typedef, mod)
    t = process_typedef(typedef, mod)
    t1, tn = t 
    return quote
        $t1
       	Overseer.component_type(::Type{$tn}) = Overseer.SharedComponent
    end
end

