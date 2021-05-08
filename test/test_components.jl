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

const c1 = Overseer.component_type(Test1){Test1}()
const c2 = Overseer.component_type(Test2){Test2}()
const c3 = Overseer.component_type(Test3){Test3}()
const c4 = Overseer.component_type(Test4){Test4}()

const entities1 = [Entity(i) for i in 2:2:10]
const entities2 = [Entity(i) for i in 10:3:20]
const entities3 = [Entity(i) for i in 3:10]
const entities4 = [Entity(1)]

@component struct ParametricComp{T}
    x::T
end

@component @with_kw struct ParametricCompKw{T}
    x::T = 1.0
end

@component struct ParametricCompFunc{T}
    x::T 
    function ParametricCompFunc()
        new{Float64}(1.0)
    end
    function ParametricCompFunc{T}() where {T}
        new{T}(T(1.0))
    end
end

@testset "Basic Component definitions" begin
    @test Overseer.component_type(Test1) == Component
    @test Overseer.component_type(Test2) == Component
    @test Overseer.component_type(Test3) == SharedComponent
    @test Overseer.component_type(Test4) == SharedComponent

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

    @test ParametricComp <: Overseer.ComponentData

    @test ParametricCompKw <: Overseer.ComponentData
    @test ParametricCompKw().x == 1.0
    
    @test ParametricCompFunc <: Overseer.ComponentData
    @test ParametricCompFunc().x == 1.0
    @test ParametricCompFunc{Int}().x === 1
    
end

@testset "Component iteration" begin
    t = 0
    for e in @entities_in(((c1 && c3) || c4) && !c2)
        t += e.id
    end
    @test t == 4+6+8+1

    t = 0
    for e in @entities_in((c1 || c3) && !c2)
        if e in c1
            t += e.id
        end
        if e in c3
            t += c3[e].p
        end
    end
    @test t == 27

    t = 0
    for e in @entities_in(c1)
        t += e.id
    end
    @test t == sum(2:2:10)
end

@testset "Component and entity manipulation" begin
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
end

@component @with_kw struct Spatial_t
    position::NTuple{3, Float64} = (1.0,1.0,1.0)
end
@component struct Spring_t
    center::NTuple{3, Float64}
end

@component struct Spatial1_t
    position::NTuple{3, Float64}
end

@component mutable struct Spatial2_t
    position::NTuple{3, Float64}
end

@testset "iteration syntax" begin
    m = Ledger()
    for i = 1:5
        Entity(m, Spatial_t(), Spring_t((i, 1.0, 2.0)), Spatial1_t((3.0, 1.0, 2.0)), Spatial2_t((1.0, 1.0, 1.0)))
    end
    for e in @entities_in(m, Spatial_t && Spring_t)
        e[Spatial_t] = Spatial_t(e.position .+ e.center)
    end
    @test m[Spatial_t][Entity(1)].position == (2.0, 2.0, 3.0)
    for e in @entities_in(m, Spatial_t && Spatial1_t)
        e[Spatial_t] = Spatial_t(e[Spatial_t].position .+ e[Spatial1_t].position)
    end
    @test m[Spatial_t][Entity(1)].position ==  (5.0, 3.0, 5.0)
    @test_throws ErrorException begin
        for e in @entities_in(m, Spatial_t && Spatial1_t)
            e[Spatial_t] = Spatial_t(e.position .+ e[Spatial1_t].position)
        end
    end
    for e in @entities_in(m, Spatial2_t )
        e.position = 2 .* e.position
    end
    @test m[Spatial2_t][Entity(1)].position == (2.0, 2.0, 2.0)
    
    for e in @entities_in(m, Spatial2_t )
        t = e[Spatial2_t]
        t.position = (0.0,0.0,0.0)
    end
    @test m[Spatial2_t][Entity(1)].position == (0.0, 0.0, 0.0)
     
end
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
