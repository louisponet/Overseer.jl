using Parameters

@component struct Test1
    p::Int
end

Test1() = Test1(0)

@component @with_kw struct Test2
    p::Int = 1
end

@shared_component struct Test3
    p::Int
end
Test3() = Test3(1)

@shared_component @with_kw struct Test4
    p::Int = 1
end

@test Overseer.component_type(Test1) == Component
@test Overseer.component_type(Test2) == Component
@test Overseer.component_type(Test3) == SharedComponent
@test Overseer.component_type(Test4) == SharedComponent

c1 = Overseer.component_type(Test1){Test1}()
c2 = Overseer.component_type(Test2){Test2}()
c3 = Overseer.component_type(Test3){Test3}()
c4 = Overseer.component_type(Test4){Test4}()

entities1 = [Entity(i) for i in 2:2:10]
entities2 = [Entity(i) for i in 10:3:20]
entities3 = [Entity(i) for i in 3:10]
entities4 = [Entity(1)]

for (c, es) in zip((c1, c2, c3, c4), (entities1, entities2, entities3, entities4))
    for e in es
        c[e] = eltype(c)()
    end
end

for (c, es) in zip((c1, c2, c3, c4), (entities1, entities2, entities3, entities4))
    for e in es
        @test in(e, c)
    end
end

t = 0
for e in @entities_in(((c1 && c3) || c4) && !c2)
    global t += e.id
end
@test t == 4+6+8+1

t = 0
for e in @entities_in((c1 || c3) && !c2)
    if e in c1
        global t += e.id
    end
    if e in c3
        global t += c3[e].p
    end
end
@test t == 27

t = 0
for e in @entities_in(c1)
    global t += e.id
end
@test t == sum(2:2:10)

@test pop!(c1, Entity(10)) == Test1()

@test length(c1) == length(entities1) - 1

@test pop!(c2, Entity(10)) == Test2()

@test length(c2) == length(entities2) - 1
@test c1[1] == Test1()
@test c3[1] == Test3()

c2[Entity(13)] = Test2(50)
@test c2[Entity(13)] == Test2(50)

c3[Entity(13)] = Test3(50)
@test c3[Entity(13)] == Test3(50)

pop!(c3, Entity(13))
@test !in(Entity(13), c3)

empty!(c1)
@test isempty(c1)

empty!(c3)
@test isempty(c3)


# swap_ordering
c2[Entity(12)] = Test2()

@test_throws BoundsError swap_order!(c2, Entity(14), Entity(15))
@test_throws BoundsError swap_order!(c2, Entity(13), Entity(14))

orig1 = c2[Entity(12)]
orig2 = c2[Entity(13)]

orig_id1 = c2.indices[12]
orig_id2 = c2.indices[13]

swap_order!(c2, Entity(12), Entity(13))
@test c2[Entity(12)] == orig1
@test c2[Entity(13)] == orig2

@test c2.indices[13] == orig_id1
@test c2.indices[12] == orig_id2

c3[Entity(12)] = Test3()
c3[Entity(13)] = Test3(50)

@test_throws BoundsError swap_order!(c3, Entity(14), Entity(15))
@test_throws BoundsError swap_order!(c3, Entity(13), Entity(14))

orig1 = c3[Entity(12)]
orig2 = c3[Entity(13)]

orig_id1 = c3.indices[12]
orig_id2 = c3.indices[13]

swap_order!(c3, Entity(12), Entity(13))
@test c3[Entity(12)] == orig1
@test c3[Entity(13)] == orig2

@test c3.indices[13] == orig_id1
@test c3.indices[12] == orig_id2

# Issue 4: collect() and iterator length
@testset "collect" begin
    e1 = Entity(1)
    e2 = Entity(2)
    e3 = Entity(3)

    comp1 = Overseer.component_type(Test1){Test1}()
    comp2 = Overseer.component_type(Test2){Test2}()
    comp1[e1] = Test1(1)
    comp1[e2] = Test1(1); comp2[e2] = Test2(1)
    comp2[e3] = Test2(1)

    iter = @entities_in(comp1 && comp2)
    es = collect(iter)
    @test getfield.(es, :e) == [e2]
    @test eltype(es) == Overseer.EntityState{Tuple{Ptr{Test1},Ptr{Test2}}}
end
