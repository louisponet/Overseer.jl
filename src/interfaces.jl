using Test

Base.@kwdef mutable struct TestCompData
    p::Int = 0
end
Base.:(==)(t::TestCompData, t1::TestCompData) = t.p == t1.p
Base.:(<)(en1::TestCompData, en2::TestCompData) = en1.p < en2.p

Base.:(<)(v, en1::TestCompData) = v < en1.p

Base.:(==)(v, en1::TestCompData) = en1.p == v

# AbstractComponent Interface
"""
Tests whether an [`AbstractComponent`](@ref) satisfies the interface.
"""
function test_abstractcomponent_interface(::Type{T}) where {T<:AbstractComponent}
    @testset "Interface: $T" begin
        @testset "Basics" begin
            c = T{TestCompData}()

            @test eltype(c) <: TestCompData
            @test indices(c) isa Indices
            @test indices_iterator(c) isa Union{Indices, IndicesIterator}
            @test reverse_indices_iterator(c) isa ReverseIndicesIterator

            @test isempty(c)
            @test length(c) == 0
            c[Entity(1)] = TestCompData(1)
            c[Entity(2)] = TestCompData(1)
            @test Entity(2) in c
            @test length(c) == 2 == size(c)[1] == length(indices(c)) == length(data(c))

            @test c[Entity(2)] isa TestCompData

            @test entity(c, 1) isa EntityState{Tuple{T{TestCompData}}}
            @test pop!(c, Entity(2)) == EntityState(Entity(2), TestCompData(1))
            @test pop!(c) == EntityState(Entity(1), TestCompData(1))
            @test isempty(c)

            c[Entity(1)] = TestCompData(1)
            c[Entity(2)] = TestCompData(2)

            # Needed for order swapping
            data(c)[1], data(c)[2] = data(c)[2], data(c)[1]
            @test c[Entity(2)] == TestCompData(1)
            @test c[Entity(1)] == TestCompData(2)

            c[Entity(1)], c[Entity(2)] = c[Entity(2)], c[Entity(1)]
            @test c[Entity(1)] == TestCompData(1)
            @test c[Entity(2)] == TestCompData(2)

            @test iterate(c)[1] isa TestCompData
            empty!(c)
            @test isempty(c)
        end
        entities1 = [[Entity(i) for i in 2:2:10]; [Entity(i) for i in 134:274:7592]]

        c = T{TestCompData}()
        
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
            
            c[EntityState(Entity(2), (c[Entity(2)],))] = TestCompData(321)
            @test c[EntityState(Entity(2), (c[Entity(2)],))] == TestCompData(321)

            c[Entity(2)] = TestCompData(2)

            t = EntityState(Entity(2), (c[Entity(2)],))
            t.p = 4
            @test c[t].p == 4
            @test c[Entity(2)].p == 4
            t.p = 2
        end
        
        @testset "Basic map, filter" begin
            @test map(e -> e.p, c) == map(e -> c[e].p, eachindex(c))
            @test filter(e -> e.p < 5, c) == c[filter(e -> c[e].p < 5, eachindex(c))]
            @test map(e -> e.p, @entities_in(c)) == map(e -> c[e].p, eachindex(c))
            @test filter(e -> e.p < 5, @entities_in(c)) == filter(e -> e.p < 5, collect(@entities_in(c)))
       end
       
        @testset "Component manipulation" begin
            @test pop!(c, Entity(10)) == EntityState(Entity(10), TestCompData(10))

            @test length(c) == length(entities1) - 1
            @test c[1] == TestCompData(2)

            c[Entity(13)] = TestCompData(50)
            @test c[Entity(13)] == TestCompData(50)

            pop!(c, Entity(13))
            @test !in(Entity(13), c)

            # swap_ordering
            c[Entity(12)] = TestCompData()

            @test_throws BoundsError swap_order!(c, Entity(14), Entity(15))
            @test_throws BoundsError swap_order!(c, Entity(13), Entity(14))

            e1 = Entity(134)
            e2 = Entity(8)
            orig1 = c[e1]
            orig2 = c[e2]

            orig_id1 = indices(c)[e1.id]
            orig_id2 = indices(c)[e2.id]

            swap_order!(c, e1, e2)
            @test c[e2] == orig2
            @test c[e1] == orig1

            @test indices(c)[e2.id] == orig_id1
            @test indices(c)[e1.id] == orig_id2

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

