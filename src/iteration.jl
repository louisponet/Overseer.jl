abstract type AbstractIndicesIterator end

########################################
#                                      #
#            IndicesIterator           #
#                                      #
########################################

struct IndicesIterator{I, T<:Function} <: AbstractIndicesIterator
    shortest::I
    test::T
end

Base.IteratorSize(::AbstractIndicesIterator) = Base.SizeUnknown()
Base.IteratorEltype(::AbstractIndicesIterator) = Base.HasEltype()
Base.in(i::Int, it::AbstractIndicesIterator) = it.test(i)

@inline indices(i::Indices) = i

@inline function Base.length(it::AbstractIndicesIterator)
    it_length = 0
    for i = 1:length(it.shortest)
        @inbounds if it.test(entity_index(it.shortest, i))
            it_length += 1
        end
    end
    return it_length
end

@inline function Base.last(it::IndicesIterator)
    for i = length(it.shortest):-1:1
        @inbounds id = entity_index(it.shortest, i)
        if it.test(id)
            return id
        end
    end
end

Base.@propagate_inbounds @inline entity_index(c::Union{<:AbstractComponent, Indices}, i::Int) = indices(c).packed[i]

@inline function Base.iterate(it::IndicesIterator, state=1)
    it_length = length(it.shortest)
    for i=state:it_length
        @inbounds id = entity_index(it.shortest, i)
        if it.test(id)
            return id, i+1
        end
    end
end

########################################
#                                      #
#        ReverseIndicesIterator        #
#                                      #
########################################

struct ReverseIndicesIterator{I, T<:Function} <: AbstractIndicesIterator
    shortest::I
    test::T
end

@inline function Base.iterate(it::ReverseIndicesIterator, state=length(it.shortest))
    # it_length = length(it.shortest)
    for i=state:-1:1
        @inbounds id = entity_index(it.shortest, i)
        if it.test(id)
            return id, i - 1
        end
    end
end

@inline function Base.last(it::ReverseIndicesIterator)
    for i = 1:length(it.shortest)
        @inbounds id = entity_index(it.shortest, i)
        if it.test(id)
            return id
        end
    end
end


for (m, it) in zip((:indices_in, :reverse_indices_in), (:IndicesIterator, :ReverseIndicesIterator))
    @eval macro $m(indices_expr)
        expr, t_sets, t_notsets, t_orsets = expand_indices_bool(indices_expr)
        return esc(quote
            sets = $(Expr(:tuple, t_sets...))
            orsets = $(Expr(:tuple, t_orsets...))

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
            $$it(shortest, x -> $expr)
        end)
    end
end

function expand_indices_bool(expr)
    if expr isa Symbol || expr.head in (:., :ref, :curly)  || (expr.head == :call && expr.args[1] != :!)
        return Expr(:call, :in, :x, expr), [expr], Symbol[], Symbol[]
    end
    # if !in(expr.head, (:||, :&&)) && !(expr.head == :call && expr.args[1] in == :!)
    #     error("Can only expand expressions with ||, && and !")
    # end
    sets = Union{Symbol, Expr}[]
    notsets = Union{Symbol, Expr}[]
    orsets = Union{Symbol, Expr}[]
    if expr.args[1] == :!
        nothing
    elseif isa(expr.args[1], Symbol)
        if expr.head != :|| 
            push!(sets, expr.args[1])
        else
            push!(orsets, expr.args[1])
        end
        expr.args[1] = Expr(:call, :in, :x, expr.args[1])
    else
        expr_, sets_, notsets_, orsets_ = expand_indices_bool(expr.args[1])
        append!(sets,  sets_)
        append!(orsets,  orsets_)
        append!(notsets,  notsets_)
        expr.args[1] = expr_
    end
    if isa(expr.args[2], Symbol)
        if expr.args[1] != :!
            if expr.head != :|| 
                push!(sets, expr.args[2])
            else
                push!(orsets, expr.args[2])
            end
        else
            push!(notsets, expr.args[2])
        end
        expr.args[2] = Expr(:call, :in, :x, expr.args[2])
    else
        expr_, sets_, notsets_, orsets_ = expand_indices_bool(expr.args[2])
        if expr.args[1] != :!
            append!(sets, sets_)
        else
            append!(notsets, sets_)
        end
        append!(orsets, orsets_)
        append!(notsets, notsets_)
        expr.args[2] = expr_
    end
    return expr, sets, notsets, orsets
end

########################################
#                                      #
#            EntityIterator            #
#                                      #
########################################
struct EntityIterator{T <: Union{AbstractIndicesIterator,Indices,AbstractGroup}, TT <: Tuple}
    it::T
    components::TT
end
   
Base.IteratorSize(::EntityIterator) = Base.SizeUnknown()
Base.IteratorEltype(::EntityIterator) = Base.HasEltype()
Base.eltype(::EntityIterator{T, TT}) where {T, TT} = EntityState{TT}
Base.length(i::EntityIterator) = length(i.it)

function Base.filter(f, it::EntityIterator)
    j = 1
    out = Vector{eltype(it)}(undef, length(it))
    for e in it
        @inbounds out[j] = e
        j = f(e) ? j+1 : j
    end
    resize!(out, j-1)
    sizehint!(out, length(out))
    return out 
end

function Base.map(f, it::EntityIterator)
    j = 1
    t, _ = iterate(it)
    tf = f(t)
    out = Vector{typeof(tf)}(undef, length(it))
    for e in it
        @inbounds out[j] = f(e)
        j += 1
    end
    resize!(out, j-1)
    sizehint!(out, length(out))
    return out 
end

Base.in(e::AbstractEntity, i::EntityIterator) = in(e.id, i.it)

@inline function Base.iterate(i::EntityIterator, state = i.it isa ReverseIndicesIterator ? length(i.it.shortest) : 1)
    n = iterate(i.it, state)
    n === nothing && return n
    @inbounds e = Entity(n[1])
    @inbounds return EntityState(e, i.components), n[2]
end

Base.last(i::EntityIterator) = EntityState(Entity(last(i.it)), i.components)

function iterator_expr(expr, t_sets, t_notsets, t_orsets, it, it_short)
    if length(t_sets) == 1 && isempty(t_orsets) && expr.args[2] isa Symbol
        return :(Overseer.EntityIterator($it_short($(t_sets[1])), ($(t_sets[1]),)))
    else
        t_comps    = gensym("t_comps")
        t_or_comps = gensym("t_or_comps")
        sets       = gensym("sets")
        t_shortest = gensym("t_shortest")
        if isempty(t_sets)
            t_shortest_expr = quote
                $sets = map(Overseer.indices_iterator, $t_or_comps)
                $t_shortest = $sets[findmin(map(length, $sets))[2]]
            end
        else
            t_shortest_expr = quote
                $sets = map(Overseer.indices_iterator, $t_comps)
                $t_shortest = $sets[findmin(map(length, $sets))[2]]
            end
        end
        if !isempty(t_orsets)
            t_shortest_expr = quote
                $t_shortest_expr
                $t_shortest = union($t_shortest, map(x->Overseer.indices_iterator(x), $t_or_comps)...)
            end
        end                    
            
        return MacroTools.flatten(quote
            $t_comps = $(Expr(:tuple, t_sets...))
            $t_or_comps = $(Expr(:tuple, t_orsets...))
            $t_shortest_expr
            Overseer.EntityIterator($it($t_shortest, x->$expr), ($t_comps..., $t_or_comps...,))
        end)
    end
end

for (m, it_short, it) in zip((:entities_in, :safe_entities_in), (:indices_iterator, :reverse_indices_iterator), (:IndicesIterator, :ReverseIndicesIterator))

    @eval macro $m(indices_expr)
        expr, t_sets, t_notsets, t_orsets = expand_indices_bool(indices_expr)
        return esc(iterator_expr(expr, t_sets, t_notsets, t_orsets, $it, $it_short))
    end

    @eval macro $m(ledger, indices_expr)
        expr, t_sets, t_notsets, t_orsets = expand_indices_bool(indices_expr)
        t_comp_defs = quote
        end
        comp_sym_map = Dict()
        for s in [t_sets; t_notsets; t_orsets]
            sym = gensym()
            t_comp_defs = quote
                $t_comp_defs
                $sym = $ledger[$s]
            end
            comp_sym_map[s] = sym
        end
        t_comp_defs = MacroTools.rmlines(MacroTools.flatten(t_comp_defs))
        
        expr = MacroTools.prewalk(expr) do x
            if x in keys(comp_sym_map)
                return comp_sym_map[x]
            else
                return x
            end
        end
        t_sets = map(x -> comp_sym_map[x], t_sets)
        t_orsets = map(x -> comp_sym_map[x], t_orsets)
        return esc(quote
            $t_comp_defs
            $(iterator_expr(expr, t_sets, t_notsets, t_orsets, $it, $it_short))
        end)
    end
end

"""
    @entities_in(comp_expr)
    @entities_in(l, comp_expr)

This macro creates an iterator that iterates over all entities that are present
in the components according to the expression passed to it. Each iteration an
[`EntityState`](@ref) is returned that refers to the entity and the associated data
inside the [Components](@ref).

`comp_expr` is a boolean expression that is used to decide which entities to return.
# Examples
```julia
for e in @entities_in(comp1 && (comp2 || comp3) && !comp4)
    # do something with e
end

for e in @entities_in(ledger, CompType1 && (CompType2 || CompType3) && !CompType4)
    # do something with e
end
```
Assuming that `comp1 = ledger[CompType1]` and similar for the others, these expressions will
loop over the [Entities](@ref) that are in `comp1`, in `comp2` or `comp3`, and not in
`comp4`.
"""
:(@entities_in)

"""
    @safe_entities_in(comp_expr)
    @safe_entities_in(ledger, comp_expr)

Similar to [`@entities_in`](@ref) but safe to [`pop!`](@ref) entities during iteration.
"""
:(@safe_entities_in)
    

########################################
#                                      #
#            EntityPoolIterator        #
#                                      #
########################################
    
struct EntityPoolIterator{T} <: AbstractVector{T}
    c::PooledComponent{T}
    pool_id::Int
end

indices_iterator(g::EntityPoolIterator) = g
indices(g::EntityPoolIterator) = g

"""
    entity_pool(c::PooledComponent, pool_id::Int)
    entity_pool(c::PooledComponent, e::AbstractEntity)

Returns an iterator that iterates over all the [`Entities`](@ref EntityState) in the pool
with `id == pool_id` or the pool to which `e` belongs.

# Example

```julia
for e in entity_pool(c, 1)
    # do something with e belonging to the first pool of c
end
```
"""
function entity_pool(c::PooledComponent, pool_id::Int)
    @boundscheck if length(c.data) < pool_id
        throw(BoundsError(c, pool_id))
    end
    return EntityPoolIterator(c, pool_id)
end
entity_pool(c::PooledComponent, e::AbstractEntity) = entity_pool(c, pool(c, e))

@inline entity_index(it::EntityPoolIterator, i::Int) =
    @inbounds it.c.indices.packed[findnext(isequal(it.pool_id), it.c.pool, i)]
    
@inline function Base.iterate(i::EntityPoolIterator, state = (1, 1))
    state[2] > i.c.pool_size[i.pool_id] && return nothing
    n = findnext(isequal(i.pool_id), i.c.pool, state[1])
    n === nothing && return n
    return EntityState(Entity(i.c.indices.packed[n]), i.c), (n + 1, state[2] + 1)
end

Base.getindex(iterator::EntityPoolIterator, i::Int) = iterate(iterator, (i, 1))[1]
    
Base.IteratorSize(::Type{EntityPoolIterator}) = Base.HasLength()
Base.IteratorEltype(::Type{EntityPoolIterator}) = Base.HasEltype()
Base.eltype(::EntityPoolIterator) = Entity
Base.eltype(::Type{EntityPoolIterator}) = Entity
Base.length(i::EntityPoolIterator) = i.c.pool_size[i.pool_id]
Base.lastindex(i::EntityPoolIterator) = length(i)
Base.size(i::EntityPoolIterator) = (length(i),)

function Base.filter(f, it::EntityPoolIterator)
    j = 1
    out = Vector{eltype(it)}(undef, length(it))
    for e in it
        @inbounds out[j] = e
        j = f(e) ? j+1 : j
    end
    resize!(out, j-1)
    sizehint!(out, length(out))
    return out 
end

Base.@propagate_inbounds @inline Base.in(e::Entity, it::EntityPoolIterator) =
    in(e, it.c) && @inbounds pool(it.c, e) == it.pool_id
Base.@propagate_inbounds @inline Base.in(e::Int, it::EntityPoolIterator) =
    in(e, it.c.indices) && @inbounds pool(it.c, e) == it.pool_id

########################################
#                                      #
#            PoolsIterator             #
#                                      #
########################################
struct PoolsIterator{T}
    c::PooledComponent{T}
end

"""
    pools(c::PooledComponent)

Returns an iterator that loops over the pools in `c`, returning a `Tuple`
with the data and an iterator like the one gotten from [`entity_pool`](@ref).

# Example
```julia
for (data, entities) in pools(c)
    for e in entities
        # do something with e
    end
end
```
"""
pools(c::PooledComponent) = PoolsIterator(c)

function Base.iterate(i::PoolsIterator, p = 1)
    p > length(i.c.data) && return nothing
    return (@inbounds i.c.data[p], EntityPoolIterator(i.c, p)), p+1
end

Base.length(i::PoolsIterator) = length(i.c)
