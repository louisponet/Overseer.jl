# ECS (Entity Component System)
[![Build Status](https://travis-ci.org/louisponet/ECS.jl.svg?branch=master)](https://travis-ci.org/louisponet/ECS.jl)
[![Coverage Status](https://coveralls.io/repos/github/louisponet/ECS.jl/badge.svg?branch=master)](https://coveralls.io/github/louisponet/ECS.jl?branch=master)

This package supplies a lightweight, performant and friction-free implementation of the [Entity component system](https://en.wikipedia.org/wiki/Entity_component_system) paradigm. It has been used mostly in game development, however I think that it's concept, way of programming and thinking can be applied and used in more broad applications. It offers a very clean and flexible way to gradually build up an application in well separated blocks, while remaining very performant due to the way data is generally structured and used.

The API and performance of this package has been evolving as I used it during development of [Glimpse](https://github.com/louisponet/Glimpse.jl).

## ECS Basics
The main idea of an ECS is to have a very clear separation between data and logic, grouping data in logic-free `Components` and logic in data-free `Systems`. Systems will perform their logic on a set of Components they care about, usually iterating through all the entities that have a particular combination of the components, systems tend to not care about specific entities, only groups of them. This allows for ideal performance since data is accessed through iterating over packed arrays, while allowing a high degree of flexibility by attaching different components to entities on the fly. 

ECS can be implemented in a lot of ways, each with slightly different behaviors. This is a small introduction to the specifics of this implementation, since it's important to understand it to be used effectively. 

### Entity
Purely an identifier, an index.

### Component & ComponentData
The data that can be attached to Entities is a subtype of `ComponentData` and is stored in `Component`. An `Entity` can be used as an index into the `Component` to retrieve its data. 
Each `ComponentData` should be purely a store for data, with no more logic attached to it than for creation and accessing. 

### System & SystemStage
This where all the logic should take place. Each system should be an empty struct [(except for maybe holding settings info)](https://github.com/louisponet/Glimpse.jl/blob/43d9e0d6f116343324b4a083d3cb80943225ac4e/src/systems/rendering/depthpeeling.jl#L18) that subtypes `System` and overloads 2 functions:
- `ECS.update(::System, m::AbstractManager)`
- `ECS.requested_components(::System)`

The first one will be used to perform each update, i.e. perform the system's main logic, while the latter is used when the system is added to an `AbstractManager` to make sure that all `Component`s that the system cares for are present.

Systems are then grouped together into a `SystemStage` which is really just a `Pair{Symbol, Vector{System}}`, which is just to allow for updating specific groups of systems together if desired.

### AbstractManager
All Entities, Components and SystemStages are grouped in an `AbstractManager` which takes care of creating new entities, accessing components, updating systems and generally making sure that everything runs.

## Example
To get a better understanding of how all of this works, it's best to see it in action in an example. 
Here we will simulate oscillation and rotation of entities. 

First we define the components that will be used.
```julia
using ECS
using GeometryTypes

@component struct Spatial
    position::Point3{Float64}
    velocity::Vec3{Float64}
end

@component struct Spring
    center::Point3{Float64}
    spring_constant::Float64
end
   
@component struct Rotation
	omega::Float64
	center::Point3{Float64}
	axis::Vec3{Float64}
end
```
One thing to remember is that for now components can not have type parameters. 
Next we define our systems.

```julia
struct Oscillator <: System end

ECS.requested_components(::Oscillator) = (Spatial, Spring)

function ECS.update(::Oscillator, m::AbstractManager)
	spatial = m[Spatial]
	spring = m[Spring]
	for e in @entities_in(spatial && spring)
		e_spat  = spatial[e]
		spr     = spring[e]
		v_prev  = e_spat.velocity 
		new_v   = v_prev - (e_spat.position - spr.center) * spr.spring_constant
		spatial[e] = Spatial(e_spat.position, new_v)
	end
end

struct Rotator <: System  end
ECS.requested_components(::Rotator) = (Spatial, Rotation)

function ECS.update(::Rotator, dio::AbstractManager)
	rotation  = dio[Rotation]
	spatial   = dio[Spatial]
	dt = 0.01
	for e in @entities_in(rotation && spatial) 
    	e_rotation = rotation[e]
    	e_spatial  = spatial[e]
		n          = e_rotation.axis
		r          = - e_rotation.center + e_spatial.position
		theta      = e_rotation.omega * dt
		nnd        = n * dot(n, r)
		spatial[e] = Spatial(Point3f0(e_rotation.center + nnd + (r - nnd) * cos(theta) + cross(r, n) * sin(theta)), e_spatial.velocity)
	end
end

struct Mover <: System end

ECS.requested_components(::Mover) = (Spatial, )

function ECS.update(::Mover, m::AbstractManager)
    dt = 0.01
    spat = m[Spatial]
    for e in @entities_in(spat)
        e_spat = spat[e]
        spat[e] = Spatial(e_spat.position + e_spat.velocity*dt, e_spat.velocity)
    end
end
```
As we can see the oscillator will cause the velocity to be inwards towards the center of the spring, 
the rotator causes just a rotation around an axis with a given rotational velocity, and the mover updates the positions 
given the velocity.

Each system iterates over the entities that have the components like given to the rules for `@entities_in`. For example 
`@entities_in(a && b || c && !d)` will iterate through all the entities that are in component `a` and `b` or `c` but not in `d`. 

Now we group these systems in a `:simulation` stage, construct a `Manager` which is a basic `AbstractManager` and generate some entities. 
```julia
stage = SystemStage(:simulation, [Oscillator(), Rotator(), Mover()])
m = Manager(stage) #this creates the Manager with the system stage, and also makes sure all requested components are added.

e1 = Entity(m, 
            Spatial(Point3(1.0, 1.0, 1.0), Vec3(0.0, 0.0, 0.0)), 
            Spring(Point3(0.0, 0.0, 0.0), 0.01))
            
e2 = Entity(m, 
            Spatial(Point3(-1.0, 0.0, 0.0), Vec3(0.0, 0.0, 0.0)), 
            Rotation(1.0, Point3(0.0, 0.0, 0.0), Vec3(1.0, 1.0, 1.0)))

e3 = Entity(m, 
            Spatial(Point3(0.0, 0.0, -1.0), Vec3(0.0, 0.0, 0.0)), 
            Rotation(1.0, Point3(0.0, 0.0, 0.0), Vec3(1.0, 1.0, 1.0)), 
            Spring(Point3(0.0, 0.0, 0.0), 0.01))
e4 = Entity(m, 
            Spatial(Point3(0.0, 0.0, 0.0), Vec3(1.0, 0.0, 0.0)))
```
So here we created 4 entities that will be acted upon by the systems in the following way:
    - e1: `Oscillator` will update the velocity and `Mover` will change it's position
    - e2: `Rotator` will update the position, `Mover` would too, but doesn't do anything since the `velocity` is 0.
    - e3: both `Ocillator` and `Rotator`, and `Mover` will act on it.
    - e4: only `Mover` will act on it and since nothing changes it's velocity it will move away from the origin forever.

Now we are ready to do an update and look at how the entities evolved.
Notice that stages are updated sequentially, and systems inside the stage too.

```julia
update(m)
m[e1] #this groups all the componentdata that is associated with e1 
m[e2]
m[e3]
m[e4]
m[Spring][e3] #accesses Spring data for entity e3
```

Entities can be deleted completely, or scheduled for later deletion:
```julia
delete!(m, e1) #instantly deletes, but is quite slow since has to check all components for whether is has e1
schedule_delete!(m, e2) #will schedule e2 for later batch deletion
delete_scheduled!(m) #executes the batch deletion
```

New data can be assigned to entities through.
```julia
m[e2] = Spring(Point3(0.0, 0.0, 0.0), 0.01)
```

Entities can be removed from a specific component through
```julia
pop!(m[Spring], e2)
```

For more examples please have a look for now in [Glimpse.jl](https://github.com/louisponet/Glimpse.jl). 

## Implementation
The implementation is heavily inspired by [EnTT](https://github.com/skypjack/entt), using slightly modified [SparseIntSets](https://juliacollections.github.io/DataStructures.jl/latest/sparse_int_set/#DataStructures.SparseIntSet-1) to track which entities hold which components.
