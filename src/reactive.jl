mutable struct ReactiveComponent{T, C<:AbstractComponent} <: AbstractComponent{T}
    component::C
    changed::Bool
end
ReactiveComponent{T}() where {T <: ComponentData} = ReactiveComponent{T, Component{T}}(Component{T}(), false)

macro reactive_component(typedef)
    return esc(Overseer._reactive_component(typedef, __module__))
end

function _reactive_component(typedef, mod)
    t = process_typedef(typedef, mod)
    t1, tn = t 
    return quote
        $t1
       	Overseer.component_type(::Type{$tn}) = Overseer.ReactiveComponent
    end
end

function Base.getproperty(c::ReactiveComponent, f::Symbol)
    if f == :component
        return getfield(c, :component)
    elseif f == :changed
        return getfield(c, :changed)
    else
        return getproperty(c.component, f)
    end
end

# Base mutating functions 
for f in (:delete!, :permute!, :setindex!, :empty!, :pop!)
    @eval function Base.$f(c::ReactiveComponent, args...)
        c.changed = true
        $f(c.component, args...)
    end
end

# Overseer mutating functions
for f in (:swap_order!, :pop_indices_data!, :ensure_entity_id!)
    @eval function $f(c::ReactiveComponent, args...)
        c.changed = true
        $f(c.component, args...)
    end
end

# Base non-mutating
for f in (:getindex, :pointer, :iterate, :sortperm, :eltype)
    @eval Base.$f(c::ReactiveComponent, args...) = $f(c.component, args...)
end    

function update_reactive(l::AbstractLedger)
    changed_comps = filter(x -> x isa ReactiveComponent && x.changed, collect(values(components(l))))
    if isempty(changed_comps)
        return
    end
    types = eltype.(changed_comps)
    systems_to_update = System[]
    for stage in stages(l)
        for sys in stage[2]
            if any(x -> x ∈ in_components(sys), types)
                push!(systems_to_update, sys)
            end
        end
    end
    for sys in systems_to_update
        update(sys, l)
    end
    for c in changed_comps
        c.changed = false
    end

    # Now recurse for possible updated components
    update_reactive(l)
end
