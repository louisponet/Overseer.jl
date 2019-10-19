module ECS
    using Parameters

    abstract type ComponentData end

    abstract type AbstractComponent{T<:ComponentData} end

    abstract type System end

    """
        Abstract type for all entity, component and system managers. In order to use the interface,
        make sure that every subtype of AbstractManager has an overload for the function `manager` that
        points towards the fields needed for functionality (see manager.jl for more info)
    """
    abstract type AbstractManager end

    include("indices.jl")
    include("entity.jl")
    include("component.jl")
    include("system.jl")
    include("manager.jl")

    export AbstractManager, Manager, System, SystemStage, Component, SharedComponent, ComponentData, Entity
    export @component, @shared_component, @component_with_kw, @shared_component_with_kw
    export @entities_in

    export update_systems, schedule_delete!, delete_scheduled!, empty_entities!, system_stage, components, entities, system_stages

end # module
