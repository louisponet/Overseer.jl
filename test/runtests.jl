using Test
using Overseer

@testset "Indices" begin include("test_indices.jl") end
@testset "Components" begin include("test_components.jl") end
@testset "Ledger" begin include("test_ledger.jl") end
@testset "Benchmark" begin include("benchmark.jl") end
