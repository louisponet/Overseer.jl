# Overseer
```@meta
CurrentModule = Overseer
```
This package supplies a lightweight, performant and julian implementation of the [Entity Component System](https://en.wikipedia.org/wiki/Entity_component_system) (ECS) programming paradigm.
It is most well known for its applications in game development, but I believe it's a programming paradigm that can benefit a broad range of applications.
It results in a very clean and flexible way to gradually build up applications in well separated blocks, while remaining inherently performant due to the way data is structured and accessed.

The API and performance of this package are being thoroughly tested in practice in the development of:
- [Glimpse.jl](https://github.com/louisponet/Glimpse.jl): a mid level rendering toolkit
- [Trading.jl](https://github.com/louisponet/Trading.jl): a comprehensive realtime trading and backtesting framework
- [RomeoDFT.jl](https://github.com/louisponet/RomeoDFT.jl): a robust global DFT based energy optimizer

## Illustrative Example

!!! note
    A [`Component`](@ref) is technically the datastructure that holds the data for a given `Type` for the [Entities](@ref) that have that data.
    We will thus use the terminology of an [`Entity`](@ref) "having a component" and "being part of a component" interchangeably.

You can think of [Entities](@ref) as simply an identifier into [Components](@ref) which are essentially `Vectors` of data. The key observation is that a
[`System`](@ref) doesn't particularly care which [`Entity`](@ref) it is handling, only that it has the right data, i.e. that it has entries in the right
[Components](@ref).

We illustrate the concept of ECS with a very basic example were a `Mover` system will change the position of [Entities](@ref) based on their velocity.

We start by defining a `Position` and `Velocity` [`Component`](@ref):
```@example initial_example
using Overseer

@component mutable struct Position
    position::Vector{Float64}
end

@component struct Velocity
    velocity::Vector{Float64}
end
```
[Systems](@ref) are represented by a `subtype` of `System`, usually these are empty since they should signify the purely **functional** part of ECS.

```@example initial_example
struct Mover <: System end

function Overseer.update(::Mover, l::AbstractLedger)
    dt = 0.01
    for e in @entities_in(l, Position && Velocity)
        e.position .+= e.velocity .* dt
    end
end
```
We see that the `Mover` system iterates through all entities that have both the `Position` and `Velocity` [`Component`](@ref) (i.e. have `data` in both components),
and updates their position.

Now we can create a [`Ledger`](@ref) which holds all the [Entities](@ref), [Systems](@ref) and [Components](@ref):
```@example initial_example
l = Ledger(Stage(:basic, [Mover()]))
```
a [`Stage`](@ref) is essentially a way to group a set of [Systems](@ref) with some additional DAG-like features.

We can then add a couple of [Entities](@ref) to our ledger
```@example initial_example
e1 = Entity(l, Position([1,0,0]), Velocity([1,1,1]))
e2 = Entity(l, Position([2,0,1]), Velocity([1,-1,1]))
e3 = Entity(l, Position([2,0,1]))
l
```
Then, we can execute all the [Systems](@ref) by calling [`update`](@ref update(::AbstractLedger)) on the ledger
```@example initial_example
update(l)
```
and look at the final positions
```@example initial_example
l[Position][e1], l[Position][e2], l[Position][e3]
```

We see that the position of `e3` did not get updated since it did not have the `Velocity` component and as such was not touched by the `Mover` system.
