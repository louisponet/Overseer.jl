# Overseer (Entity Component System)
[![Build Status](https://github.com/louisponet/Overseer.jl/workflows/CI/badge.svg)](https://github.com/louisponet/Overseer.jl/actions?query=workflow%3ACI)
[![codecov](https://codecov.io/gh/louisponet/Overseer.jl/branch/master/graph/badge.svg?token=mVK0aEQGuu)](https://codecov.io/gh/louisponet/Overseer.jl)

This package supplies a lightweight, performant and friction-free implementation of the [Entity component system](https://en.wikipedia.org/wiki/Entity_component_system)(ECS) paradigm. It has been used mostly in game development, however I think that it's concept, way of programming and thinking can be applied and used in more broad applications. It offers a very clean and flexible way to gradually build up an application in well separated blocks, while remaining very performant due to the way data is generally structured and used.

The API and performance of this package has been evolving as I used it during development of [Glimpse](https://github.com/louisponet/Glimpse.jl).

## ECS Basics
The main idea of an ECS is to have a very clear separation between data and logic, grouping data in logic-free `Components` and logic in data-free `Systems`. Systems will perform their logic on a set of Components they care about, usually iterating through all the entities that have a particular combination of the components, systems tend to not care about specific entities, only groups of them. This allows for ideal performance since data is accessed through iterating over packed arrays, while allowing a high degree of flexibility by attaching different components to entities on the fly. 

ECS can be implemented in a lot of ways, each with slightly different behaviors. This is a small introduction to the specifics of this implementation, since it's important to understand it to be used effectively. 

### Entity
Purely an identifier, used as an index.

### Component & ComponentData
The data that can be attached to Entities is a subtype of `ComponentData` and is stored contiguously in a `Component`. An `Entity` can be used as an index into the `Component` to retrieve its data. 
Each `ComponentData` should be purely a store for data, with no more logic attached to it than for creation and accessing. 

### System & Stage
This where all the logic should take place. Each system should be an empty struct [(except for maybe holding settings info)](https://github.com/louisponet/Glimpse.jl/blob/43d9e0d6f116343324b4a083d3cb80943225ac4e/src/systems/rendering/depthpeeling.jl#L18) that subtypes `System` and overloads 2 functions:
    - `Overseer.update(::System, m::AbstractLedger)`
    - `Overseer.requested_components(::System)`

The first one will be used to perform each update, i.e. perform the system's main logic, while the latter is used when the system is added to an `AbstractLedger` to make sure that all `Component`s that the system cares for are present.

Systems are then grouped together into a `Stage` which is really just a `Pair{Symbol, Vector{System}}`, which is just to allow for updating specific groups of systems together if desired.

### AbstractLedger
All Entities, Components and Stages are grouped in an `AbstractLedger` which takes care of creating new entities, accessing components, updating systems and generally making sure that everything runs.

## Example
To get a better understanding of how all of this works, it's best to see it in action in an example. 
Here we will simulate oscillation and rotation of entities. 

First we define the components that will be used.
```julia
using Overseer
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
Next we define our systems.

```julia
struct Oscillator <: System end

Overseer.requested_components(::Oscillator) = (Spatial, Spring)

function Overseer.update(::Oscillator, m::AbstractLedger)
	for e in @entities_in(m, Spatial && Spring)
		new_v   = e.velocity - (e.position - e.center) * e.spring_constant
		e[Spatial] = Spatial(e.position, new_v)
	end
end

struct Rotator <: System  end
Overseer.requested_components(::Rotator) = (Spatial, Rotation)

function Overseer.update(::Rotator, m::AbstractLedger)
	dt = 0.01
	for e in @entities_in(m, Rotation && Spatial) 
		n          = e.axis
		r          = - e.center + e.position
		theta      = e.omega * dt
		nnd        = n * dot(n, r)
		e[Spatial] = Spatial(Point3f0(e.center + nnd + (r - nnd) * cos(theta) + cross(r, n) * sin(theta)), e.velocity)
	end
end

struct Mover <: System end

Overseer.requested_components(::Mover) = (Spatial, )

function Overseer.update(::Mover, m::AbstractLedger)
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

Each system iterates over the entities that have the components like given to the rules for `@entities_in`. 
There are two ways of using this, either in the form `@entities_in(ledger, ComponentData1 && Componentdata2)` or 
`@entities_in(comp1 && comp2)` where `comp1 = m[ComponentData1]`,`comp2 = m[ComponentData2]`. 
Rules can be given in the form of `@entities_in(a && (b || c) && !d)`, which will iterate through 
all the entities that are in component `a` and `b` or `c` but not in `d`. 

Now we group these systems in a `:simulation` stage, construct a `Ledger` which is a basic `AbstractLedger` and generate some entities. 
```julia
stage = Stage(:simulation, [Oscillator(), Rotator(), Mover()])
m = Ledger(stage) #this creates the Overseer with the system stage, and also makes sure all requested components are added.

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
