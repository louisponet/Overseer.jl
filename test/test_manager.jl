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

empty!(m)
for i=1:10
    e1 = Entity(m, 
                Test1(length(entities(m))),
                Test2(length(entities(m))),
                Test3(length(entities(m))))

    e1 = Entity(m, 
                Test1(length(entities(m))),
                Test2(length(entities(m))))
                
    e2 = Entity(m, 
                Test2(length(entities(m))),
                )
end

t2 = group(m, Test1, Test2)
t1 = group(m, Test1, Test2, Test3)

@test_throws ArgumentError group(m, Test1, Test3)

@test length(groups(m)) == 1
@test t1.child == t2
@test t1.child.parent == t1 == t2.parent

@test length(t1) == 10
@test length(t2) == 20

@test m[Test1].indices.packed[1:length(t1)] == m[Test2].indices.packed[1:length(t1)] == m[Test3].indices.packed[1:length(t1)] == t1.indices.packed[1:length(t1)]
@test m[Test1].indices.packed[1:length(t2)] == m[Test2].indices.packed[1:length(t2)]  == t2.indices.packed[1:length(t2)]

test_entity = last(entities(m))
t2[test_entity] = Test1()
@test length(t1) == 10
t1[test_entity] = Test3()
@test length(t1) == 11

@test t1[test_entity] == (m[Test1][test_entity], m[Test2][test_entity], m[Test3][test_entity])

tot = 0
for e in @entities_in(group(m, Test1, Test2))
    global tot += 1
end
@test tot == length(group(m, Test1, Test2))

test_entity = entities(m)[end-1]
m[test_entity] = Test3(test_entity.id)

@test m[Test1].indices.packed[1:length(t1)] == m[Test2].indices.packed[1:length(t1)] == m[Test3].indices.packed[1:length(t1)] == t1.indices.packed[1:length(t1)]
@test m[Test1].indices.packed[1:length(t2)] == m[Test2].indices.packed[1:length(t2)]  == t2.indices.packed[1:length(t2)]

@test t1[test_entity] == (m[Test1][test_entity], m[Test2][test_entity], m[Test3][test_entity])


# tot = 0
# for e in @entities_in(group(m, Test2, Test3))
#     global tot += 1
# end
# @test tot == 8 == length(group(m, Test2, Test3))

# tot = 0
# for e in @entities_in(group(m, Test2, Test3) && group(m, Test1, Test2))
#     global tot += 1
# end

# tot2 = 0
# for e in @entities_in(m[Test1] && m[Test2] && m[Test3])
#     global tot2 += 1
# end
# @test tot == tot2

# remove_group!(m, Test2, Test3)
# @test length(groups(m)) == 2

# tg = create_group!(m, Test1, Test2; ordered=true)
# @test length(groups(m)) == 2

# @test groups(m)[1] isa ECS.OrderedGroup

# tg = group(m, Test1, Test2, Test3)
# beforelen = length(tg)
# m[Test3].shared[m[Test3].data[beforelen+1]] != Test3(5)

# m[Entity(1)] = Test3(5)
# @test length(tg) == beforelen+1
# @test m[Test3].shared[m[Test3].data[length(tg)]] == Test3(5)
# @test m[Test2].data[length(tg)] == Test2(0)








