using Parameters
using Overseer: EntityState

@with_kw struct Test1 <: ComponentData
    p::Int = 0
end

@with_kw struct Test2 <: ComponentData
    p::Int = 1
end

@with_kw struct Test3 <: ComponentData
    p1::Int = 1
end

@with_kw struct Test4 <: ComponentData
    p::Int = 1
end

for CT in (Component, PooledComponent)
    @testset "Interface: $CT" begin
        @testset "AbstractComponent interface" begin Overseer.test_abstractcomponent_interface(CT) end
        
        entities1 = [[Entity(i) for i in 2:2:10]; [Entity(i) for i in 134:274:7592]]

        c = CT{Test1}()
        
        @testset "Creation and in" begin 
            for e in entities1
                c[e] = eltype(c)(e.id)
            end
            for e in entities1
                @test in(e, c)
            end
        end

        @testset "Basic iteration" begin
            it = @entities_in(c)
            @test iterate(it)[1] == EntityState(Entity(2), (c[Entity(2)],))
            s = 0
            for e in it
                s += e.p
            end
            @test s == sum(x->c[x].p, eachindex(c))
            
            c[EntityState(Entity(2), (c[Entity(2)],))] = Test1(321)
            @test c[EntityState(Entity(2), (c[Entity(2)],))] == Test1(321)

            c[Entity(2)] = Test1(2)
        end
        
        @testset "Basic map, filter" begin
            @test map(e -> e.p, c) == map(e -> c[e].p, eachindex(c))
            @test filter(e -> e.p < 5, c) == c[filter(e -> c[e].p < 5, eachindex(c))]
            @test map(e -> e.p, @entities_in(c)) == map(e -> c[e].p, eachindex(c))
            @test filter(e -> e.p < 5, @entities_in(c)) == filter(e -> e.p < 5, collect(@entities_in(c)))
       end
       
        @testset "Component manipulation" begin
            @test pop!(c, Entity(10)) == Test1(10)

            @test length(c) == length(entities1) - 1
            @test c[1] == Test1(2)

            c[Entity(13)] = Test1(50)
            @test c[Entity(13)] == Test1(50)

            pop!(c, Entity(13))
            @test !in(Entity(13), c)

            # swap_ordering
            c[Entity(12)] = Test1()

            @test_throws BoundsError swap_order!(c, Entity(14), Entity(15))
            @test_throws BoundsError swap_order!(c, Entity(13), Entity(14))

            e1 = Entity(134)
            e2 = Entity(8)
            orig1 = c[e1]
            orig2 = c[e2]

            orig_id1 = c.indices[e1.id]
            orig_id2 = c.indices[e2.id]

            swap_order!(c, e1, e2)
            @test c[e2] == orig2
            @test c[e1] == orig1

            @test c.indices[e2.id] == orig_id1
            @test c.indices[e1.id] == orig_id2

            es = map(x->x.e, @entities_in(c))
            cur = c[es]
            pvec = reverse(es)
            permute!(c, pvec)
            @test reverse(cur) == c[es]
            
            empty!(c)
            @test isempty(c)
        end
    end
end
for (CT1, CT2) in ((Component, PooledComponent, Component, PooledComponent), (Component, PooledComponent, PooledComponent, Component))
    @testset "Combined: $CT1, $CT2" begin
        entities1 = [Entity(i) for i in 2:2:10]
        entities2 = [Entity(i) for i in 10:3:20]
        entities3 = [Entity(i) for i in 3:10]   
        entities4 = [Entity(1)]
        c1 = CT1{Test1}()
        c2 = CT1{Test2}()
        c3 = CT2{Test3}()
        c4 = CT2{Test4}()        

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

        @testset "Iteration" begin
            t = 0
            for e in @entities_in(((c1 && c3) || c4) && !c2)
                t += e.id
            end
            @test t == 4+6+8+1
            
            t = 0
            for e in @safe_entities_in(((c1 && c3) || c4) && !c2)
                t += e.id
            end
            @test t == 4+6+8+1
            # @test
            @test map(x->x.id, collect(@safe_entities_in(((c1 && c3) || c4) && !c2))) == [1, 8, 6, 4]
            t = 0
            for e in @entities_in(c1 || c3)
                if e in c1
                    t += e.id
                end
                if e in c3
                    t += e.p1
                end
            end
            @test t == 38

            t = 0
            for e in @entities_in((c1 || c3) && !c2)
                if e in c1
                    t += e.id
                end
                if e in c3
                    t += e.p1
                end
            end
            @test t == 27

            t = 0
            for e in @entities_in(c1)
                t += e.id
            end
            @test t == sum(2:2:10)

            tc = deepcopy(c1)
            t = 0
            for e in @entities_in(tc)
                t += e.id
                if e.id == 4
                    pop!(tc, e)
                end
            end
            @test t == sum(2:2:10) - 10
            
            tc = deepcopy(c1)
            t = 0
            for e in @safe_entities_in(tc)
                t += e.id
                if e.id == 4
                    pop!(tc, e)
                end
            end
            @test t == sum(2:2:10)
            
        end
        
        for (c, es) in zip((c1, c3), (entities1, entities3))
            for e in es
                c[e] = eltype(c)(e.id)
            end
        end
        @testset "map & filter" begin
            @test map(e -> e[Test1].p + e[Test3].p1, @entities_in(c1 && c3)) == map(e -> e[Test1].p + e[Test3].p1, collect(@entities_in(c1 && c3)))
            @test filter(e -> e[Test1].p + e[Test3].p1 < 7, @entities_in(c1 && c3)) == filter(e -> e[Test1].p + e[Test3].p1 < 7, collect(@entities_in(c1 && c3)))
         
        end
        @testset "collect" begin
            e1 = Entity(1)
            e2 = Entity(2)
            e3 = Entity(3)

            comp1 = CT1{Test1}()
            comp2 = CT2{Test2}()
            comp1[e1] = Test1(1)
            comp1[e2] = Test1(1); comp2[e2] = Test2(1)
            comp2[e3] = Test2(1)

            iter = @entities_in(comp1 && comp2)
            es = collect(iter)
            @test getfield.(es, :e) == [e2]
            @test eltype(es) == Overseer.EntityState{Tuple{Component{Test1},PooledComponent{Test2}}}
        end
    end
end

@testset "PooledComponent" begin
    @pooled_component struct Test5
        x::Int
    end

    @test Overseer.component_type(Test5) == Overseer.PooledComponent{Test5}
    c5 = Overseer.component_type(Test5)()

    p1 = Entity(1)
    p2 = Entity(2)
    c5[p1] = Test5(1)
    c5[p2] = Test5(2)

    entities = [Entity(i) for i in 3:10]
    for (i, e) in enumerate(entities)
        c5[e] = (p1, p2)[mod1(i, 2)]
    end

    # check created values
    count = 0
    _sum = 0
    for e in @entities_in(c5)
        count += 1
        _sum += c5[e].x
    end
    @test count == 10
    @test _sum == 15
    @test c5.pool_size == [5, 5]

    # check for no duplication
    @test length(c5.data) == 2

    # Check some basics
    @test p1 in c5
    @test pop!(c5, p1) == Test5(1)
    @test !(p1 in c5)
    @test length(c5.pool) == 9
    @test c5[p2] == Test5(2)
    @test !isempty(c5)
    @test c5.pool_size == [4, 5]

    count = 0
    _sum = 0
    for e in @entities_in(c5)
        count += 1
        _sum += c5[e].x
    end
    @test count == 9
    @test _sum == 14

    # adjust parent value of pool
    c5[parent(p2)] = Test5(1)
    count = 0
    _sum = 0
    for e in @entities_in(c5)
        count += 1
        _sum += c5[e].x
    end
    @test count == 9
    @test _sum == 9
    @test c5.pool_size == [4, 5]

    # adjust single value
    c5[p2] = Test5(2)
    count = 0
    _sum = 0
    for e in @entities_in(c5)
        count += 1
        _sum += c5[e].x
    end
    @test count == 9
    @test _sum == 10
    @test length(c5.data) == 3
    @test c5.pool_size == [4, 4, 1]

    Overseer.make_unique!(c5)
    @test length(c5.data) == 2
    @test c5.pool_size == [1, 8]
    @test c5.data == [Test5(2), Test5(1)]

    # remove all entites of a pool
    for i in 3:10
        pop!(c5, Entity(i))
    end
    @test length(c5.data) == 1
    @test length(c5) == 1
    @test c5.pool_size == [1]

    empty!(c5)
    @test isempty(c5)
    @test c5.pool_size == Int[]

    e1 = Entity(1)
    e2 = Entity(2)
    e3 = Entity(3)
    c5[e1] = Test5(1)
    c5[e3] = e1
    c5[e2] = Test5(10)

    @test length(collect(entity_pool(c5, 1))) == c5.pool_size[1]
    @test collect(entity_pool(c5, 1)) == [Entity(1), Entity(3)]

    c1 = Component{Test1}()
    c1[e1] = Test1(1)
    c1[e2] = Test1(2)
    c1[e3] = Test1(3)
    order = Entity[]
    for e in @entities_in(entity_pool(c5, 1) && c1)
        push!(order, e.e)
    end
    for e in @entities_in(entity_pool(c5, 2) && c1)
        push!(order, e.e)
    end
    @test order == [Entity(1), Entity(3), Entity(2)]

    
    @test filter(x-> x.id <= 1, entity_pool(c5, 1)) == [Entity(1)]

    @test length(pools(c5)) == length(c5)
    @test iterate(pools(c5))[1][1] == Test5(1) 
    @test parent(c5, 2) == Entity(2)
    @test parent(c5, 1) == Entity(1)
    @test parent(c5, Entity(3)) == Entity(1)

    empty!(c5)
    @test isempty(c5)
end

@component struct Comp
    x::Int
end
@pooled_component struct PooledComp
    x::Int
end

@component struct ParametricComp{T}
    x::T
end
@pooled_component struct PooledParametricComp{T}
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

@testset "Macros" begin
    @test Overseer.component_type(ParametricComp{Float64})       == Component{ParametricComp{Float64}}
    @test Overseer.component_type(PooledParametricComp{Float64}) == PooledComponent{PooledParametricComp{Float64}}
    @test Overseer.component_type(Comp)       == Component{Comp}
    @test Overseer.component_type(PooledComp) == PooledComponent{PooledComp}

    @test ParametricComp <: Overseer.ComponentData

    @test ParametricCompKw <: Overseer.ComponentData
    @test ParametricCompKw().x == 1.0
    
    @test ParametricCompFunc <: Overseer.ComponentData
    @test ParametricCompFunc().x == 1.0
    @test ParametricCompFunc{Int}().x === 1
end

