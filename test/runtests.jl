using Test
using ECS

@testset "Indices" begin include("test_indices.jl") end
@testset "Components" begin include("test_components.jl") end
@testset "Manager" begin include("test_manager.jl") end

