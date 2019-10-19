using ECS

@component struct Test1 end
@component struct Test2 end
@component struct Test3 end
@component struct Test4 end

struct TestSys <: System end

ECS.requested_components(::TestSys) = (Test1, Test2, Test3, Test4)

function ECS.update(::TestSys, m::AbstractManager)
    t1 = m[Test1]
    t2 = m[Test2]
    t3 = m[Test3]
    t4 = m[Test4]
    for e in @entities_in(t1 && t2 && t3)
        t4[e] = Test4()
    end
end

m = Manager(SystemStage(:default, [TestSys()]))

Entity(m, Test1(), Test2())
Entity(m, Test2(), Test3())
for i = 1:10
    Entity(m, Test1(), Test2(), Test3())
end

@test length(m.entities) == 12

@test length(m[Test1]) == 11

update_systems(m)

@test length(m[Test4]) == 10

delete!(m, Entity(4))

@test length(m.free_entities) == 1
@test length(filter(x->x==Entity(0), m.entities)) == 1

@test length(m[Test4]) == 9

for i = 5:10
    schedule_delete!(m, Entity(i))
end
delete_scheduled!(m)

@test length(m[Test4]) == 3


empty!(m)
@test isempty(m.entities)
@test isempty(m.system_stages)
@test isempty(m.components)

push!(m, SystemStage(:default, [TestSys()]))

@test length(m.components) == 4

struct TestSys2 <: System end

push!(m, :default, TestSys())

@test length(last(system_stage(m, :default))) == 2

insert!(m, :default, 1, TestSys2())

@test last(system_stage(m, :default))[1] == TestSys2()

insert!(m, 1, SystemStage(:test, [TestSys(), TestSys2()]))

@test first(m.system_stages[1]) == :test

