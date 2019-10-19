using ECS: Component, SharedComponent, Entity
using ECS: @component, @component_with_kw, @shared_component, @shared_component_with_kw, component_id, component_type, @entities_in


@component struct Test1 end

@component_with_kw struct Test2
    p::Int = 1
end

@shared_component struct Test3 end

@shared_component_with_kw struct Test4
    p::Int = 1
end

@test component_id(Test1) == 1
@test component_id(Test2) == 2
@test component_id(Test3) == 3
@test component_id(Test4) == 4

@test component_type(Test1) == Component
@test component_type(Test2) == Component
@test component_type(Test3) == SharedComponent
@test component_type(Test4) == SharedComponent

c1 = ECS.component_type(Test1){Test1}()
c2 = ECS.component_type(Test2){Test2}()
c3 = ECS.component_type(Test3){Test3}()
c4 = ECS.component_type(Test4){Test4}()

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
for e in @entities_in((c1 || c2) && !c3)
    if e in c1
        global t += e.id
    end
    if e in c2
        global t += c2[e].p
    end
end
@test t == 5

@test pop!(c1, Entity(10)) == Test1()

@test length(c1) == length(entities1) - 1

@test pop!(c2, Entity(10)) == Test2()

@test length(c2) == length(entities2) - 1

empty!(c1)
@test isempty(c1)
