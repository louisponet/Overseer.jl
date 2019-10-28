module ECS
    using Parameters

    abstract type ComponentData end

    abstract type AbstractComponent{T<:ComponentData} end
    abstract type AbstractGroup end

    abstract type System end

    """
        Abstract type for all entity, component and system overseers. In order to use the interface,
        make sure that every subtype of AbstractOverseer has an overload for the function `overseer` that
        points towards the fields needed for functionality (see overseer.jl for more info)
    """
    abstract type AbstractOverseer end

    include("utils.jl")
    include("indices.jl")
    include("entity.jl")
    include("component.jl")
    include("group.jl")
    include("system.jl")
    include("overseer.jl")

    export AbstractOverseer, Overseer, System, SystemStage, Component, SharedComponent, ComponentData, Entity
    export @component, @shared_component, @component_with_kw, @shared_component_with_kw
    export @entities_in

    export update, schedule_delete!, delete_scheduled!, empty_entities!, system_stage, components, entities, system_stages
    export prepare, singleton, valid_entities, groups, group, create_group!, regroup!, remove_group!

end # module
