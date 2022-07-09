module Overseer
using MacroTools

abstract type AbstractComponent{T} <: AbstractVector{T} end
abstract type AbstractGroup end

"""
    System

Systems represent the part of ECS where the actual "work" happens,
by overloading the [`Overseer.update`](@ref) function for a specific
`System`, with the signature `Overseer.update(::System, m::AbstractLedger)`.
When updating a [`Ledger`](@ref) the `update` function of each system will
be called. By overloading [`Overseer.requested_components`](@ref), for a
`System`, it is ensured that those components are present in the [`Ledger`](@ref) that holds the `System`. 
Following the ECS design, a `System` should not hold data except for maybe
some settings parameters.
"""
abstract type System end

"""
    AbstractLedger

Abstract type for all ECS ledgers. In order to use the interface,
make sure that every subtype of `AbstractLedger` has an overload for the function ``ledger` that
points towards the fields needed for functionality (see ledger.jl for more info).
"""
abstract type AbstractLedger end

abstract type AbstractEntity end

include("indices.jl")
include("entity.jl")
include("component.jl")
include("group.jl")
include("system.jl")
include("ledger.jl")
include("iteration.jl")


export AbstractLedger, Ledger, System, Stage, Component, PooledComponent, Entity, entity
export @component, @pooled_component
export @safe_entities_in, @entities_in, entity_pool, pools, pool

export update, schedule_delete!, delete_scheduled!, empty_entities!, stage, components, entities, stages
export prepare, singleton, valid_entities, groups, group, create_group!, regroup!, remove_group!

# Components
export swap_order!
end # module
