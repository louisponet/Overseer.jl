using Test
using ECS

@testset "Indices" begin include("test_indices.jl") end
@testset "Components" begin include("test_components.jl") end
@testset "Overseer" begin include("test_overseer.jl") end

