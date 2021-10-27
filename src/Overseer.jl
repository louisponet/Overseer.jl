module Overseer
    using MacroTools
    
    abstract type ComponentData end

    abstract type AbstractComponent{T<:ComponentData} end
    abstract type AbstractGroup end

    abstract type System end

    """
        Abstract type for all entity, component and system ledgers. In order to use the interface,
        make sure that every subtype of AbstractLedger has an overload for the function `ledger` that
        points towards the fields needed for functionality (see ledger.jl for more info).
    """
    abstract type AbstractLedger end
    
    abstract type AbstractEntity end

    include("utils.jl")
    include("indices.jl")
    include("entity.jl")
    include("component.jl")
    include("group.jl")
    include("system.jl")
    include("ledger.jl")
    include("reactive.jl")

    export AbstractLedger, Ledger, System, Stage, Component, SharedComponent, GroupedComponent, ComponentData, Entity
    export @component, @shared_component, @grouped_component
    export @entities_in, entity_group

    export update, schedule_delete!, delete_scheduled!, empty_entities!, stage, components, entities, stages
    export prepare, singleton, valid_entities, groups, group, create_group!, regroup!, remove_group!

    # Components
    export swap_order!
end # module
