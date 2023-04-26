module Overseer
using MacroTools

"""
    AbstractComponent

Abstract type for all [Components](@ref). For now the only two [`AbstractComponents`](@ref AbstractComponent) are [`Component`](@ref) and [`PooledComponent`](@ref).
Most functionality that is defined for the [`AbstractComponent`](@ref) type assumes that there is a `.indices` member field that is of type
[`Indices`](@ref).

If this is not the case look at those functions in the `src/component.jl` file.

[`Overseer.test_abstractcomponent_interface`](@ref) is an interface test function that you can call to test a new [`AbstractComponent`](@ref)
implementation. See that function for more details.
"""
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

Abstract type for all ECS ledgers. The easiest way to use the interface is by including a standard
[`Ledger`](@ref) as a member field and defining [`ledger`](@ref) for your new [`AbstractLedger`](@ref)
type to return that field. 
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


export AbstractLedger, Ledger, System, Stage, Component, PooledComponent, Entity, entity, last_entity
export @component, @pooled_component
export @safe_entities_in, @entities_in, entity_pool, pools, pool

export update, schedule_delete!, delete_scheduled!, empty_entities!, stage, components, entities, stages
export prepare, singleton, valid_entities

# Components
export swap_order!
end # module
