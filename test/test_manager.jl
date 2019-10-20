using ECS

abstract type TComp <: ComponentData end

@component struct T1 <: TComp end
@component struct T2 end
@component struct T3 <: TComp end
@component struct T4 end

struct TSys <: System end

function ECS.prepare(::TSys, m::AbstractManager)
    if isempty(entities(m))
        Entity(m, T4())
    end
end

ECS.requested_components(::TSys) = (T1, T2, T3, T4)

function ECS.update(::TSys, m::AbstractManager)
    t1 = m[T1]
    t2 = m[T2]
    t3 = m[T3]
    t4 = m[T4]
    for e in @entities_in(t1 && t2 && t3)
        t4[e] = T4()
    end
end

m = Manager(SystemStage(:default, [TSys()]))

Entity(m, T1(), T2())
Entity(m, T2(), T3())
for i = 1:10
    Entity(m, T1(), T2(), T3())
end

@test length(valid_entities(m)) == 13

@test length(m[T1]) == 11

update(m)

@test length(m[T4]) == 11

empty!(m[T4])
update(system_stage(m, :default), m)
@test length(m[T4]) == 10

@test length(m[Entity(4)]) == 4

delete!(m, Entity(4))
@test !isempty(ECS.free_entities(m))
@test length(filter(x->x==Entity(0), m.entities)) == 1

Entity(m, T3())
@test m[T3][Entity(4)] == T3() 
@test isempty(ECS.free_entities(m))


@test length(m[T4]) == 9

for i = 5:10
    schedule_delete!(m, Entity(i))
end
delete_scheduled!(m)

@test length(m[T4]) == 3


empty!(m)
@test isempty(m.entities)
@test isempty(m.system_stages)
@test isempty(m.components)

push!(m, SystemStage(:default, [TSys()]))

@test length(m.components) == 8

struct TSys2 <: System end

push!(m, :default, TSys())

@test length(last(system_stage(m, :default))) == 2

insert!(m, :default, 1, TSys2())

@test last(system_stage(m, :default))[1] == TSys2()

insert!(m, 1, SystemStage(:test, [TSys(), TSys2()]))

@test first(m.system_stages[1]) == :test

@test eltype(m[T4]) == T4

prepare(m)
@test !isempty(entities(m))
@test singleton(m, T4) == T4()


struct SmallSys <: System end

ECS.requested_components(::SmallSys) = (T1, T3)

m2 = Manager(SystemStage(:default, [SmallSys()]))

@test m2.components[2] === ECS.EMPTY_COMPONENT

e = Entity(m2)
m2[e] = T2()

empty_entities!(m2)
@test isempty(m2.entities)
@test !isempty(m2.components)


@test length(components(m2, TComp)) == 2



