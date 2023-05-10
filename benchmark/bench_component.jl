module BenchComponent
    
using BenchmarkTools
import Random
using Overseer

@component Base.@kwdef struct Spatial
    position::NTuple{3, Float64} = (1.0,1.0,1.0)
    velocity::NTuple{3, Float64} = (1.0,1.0,1.0)
end

@component struct Spring
    center::NTuple{3, Float64}
    spring_constant::Float64
end

@component mutable struct Rotation
    omega::Float64
    center::NTuple{3, Float64}
    axis::NTuple{3, Float64}
    function Rotation()
        new(1.0, (2.0,2.0,2.0), (2.0,2.0,2.0))
    end
end

struct Oscillator <: System end

Overseer.requested_components(::Oscillator) = (Spatial, Spring)

function Overseer.update(::Oscillator, m::AbstractLedger)
    spatial = m[Spatial]
    spring = m[Spring]
    # g = group(m, Spatial, Spring)
    @inbounds for e in @entities_in(spatial && spring)
        e_spat  = spatial[e]
        spr     = spring[e]
        v_prev  = e_spat.velocity 
        new_v   = v_prev .- (e_spat.position .- spr.center) .* spr.spring_constant
        spatial[e] = Spatial(e_spat.position, new_v)
    end
end

struct Rotator <: System  end
Overseer.requested_components(::Rotator) = (Spatial, Rotation)

function Overseer.update(::Rotator, dio::AbstractLedger)
    rotation  = dio[Rotation]
    spatial   = dio[Spatial]
    dt = 0.01
    @inbounds for e in @entities_in(rotation && spatial) 
        e_rotation = rotation[e]
        e_spatial  = spatial[e]
        n          = e_rotation.axis
        r          = e_spatial.position .- e_rotation.center 
        theta      = e_rotation.omega * dt
        nnd        = n .* sum(n .* r)
        t = (r[2] * n[3] - r[3] * n[2], r[3]*n[1] - r[1] * n[3] , r[1] * n[2] - r[2] * n[1])
        spatial[e] = Spatial(e_rotation.center .+ nnd .+ (r .- nnd) .* cos(theta) .+ t .* sin(theta), e_spatial.velocity)
    end
end

struct Mover <: System end

Overseer.requested_components(::Mover) = (Spatial, )

function Overseer.update(::Mover, m::AbstractLedger)
    dt = 0.01
    spat = m[Spatial]
    @inbounds for e in @entities_in(spat)
        e_spat = spat[e]
        spat[e] = Spatial(e_spat.position .+ e_spat.velocity.*dt, e_spat.velocity)
    end
end


# Benchmark defs
const suite = BenchmarkGroup()

Random.seed!(1234)

function bench_insertion(c::Component, ids::Vector{Entity}, v)
    for e in ids
        c[e] = v
    end
    return c
end
function bench_access(c::Component, ids::Vector{Entity}, v)
    for e in ids
        v += c[e].position[1]
    end
    return v
end
function bench_access_inbounds(c::Component, ids::Vector{Entity}, v)
    @inbounds for e in ids
        v += c[e].position[1]
    end
    return v
end
function bench_delete(c::Component, ids::Vector{Entity})
    delete!(c, ids)
    return c
end

suite["basic"] = BenchmarkGroup()

const ids = Entity.(unique(rand(1:1000, 1000)))

suite["basic"]["insertion"] = @benchmarkable bench_insertion(c, $ids, $(Spatial((123.0, 1.2, 1.0), (0.4, 12.0, 234.9)))) setup=(c=Component{Spatial}()) evals=1

suite["basic"]["access"] = @benchmarkable bench_access(c, $ids, 0.0) setup=(c = Component{Spatial}(); bench_insertion(c, $ids, Spatial((123.0, 1.2, 1.0), (0.4, 12.0, 234.9))))

const c_full = Component{Spatial}()
bench_insertion(c_full, ids, Spatial((123.0, 1.2, 1.0), (0.4, 12.0, 234.9)))

suite["basic"]["access inbounds"] = @benchmarkable bench_access_inbounds(c, $ids, 0.0) setup=(c = deepcopy(c_full))

suite["basic"]["deletion"] = @benchmarkable bench_delete(c, $ids) setup=(c = deepcopy(c_full)) evals=1

suite["real life"] = BenchmarkGroup()
suite["real life"]["creation"] = BenchmarkGroup()

st = Stage(:simulation, [Oscillator(), Rotator(), Mover()])

suite["real life"]["creation"]["ledger"] = @benchmarkable m = Ledger($st)
function e_fill(m)
    for i=1:1000
        e1 = Entity(m, 
                    Spatial((i, 1.0, 1.0), (0.0, 0.0, 0.0)),
                    Spring((1.0, 0.0, 0.0), 0.01))
                   
        e1 = Entity(m, 
                    Spatial((i, 1.0, 1.0), (0.0, 0.0, 0.0)),
                    Spring((1.0, 0.0, 0.0), 0.01),
                    Rotation())
                   
    end
end

suite["real life"]["creation"]["filling entities"] = @benchmarkable e_fill(m) setup=(m = Ledger(st))

suite["real life"]["update"] = BenchmarkGroup()

suite["real life"]["update"]["old school empty"] = @benchmarkable update(m) setup=(m = Ledger(st))
suite["real life"]["update"]["old school full"] = @benchmarkable update(m) setup=(m = Ledger(st); e_fill(m))

function update_new(::Oscillator, m::AbstractLedger)
    @inbounds for e in @entities_in(m, Spatial && Spring)
        new_v   = e.velocity .- (e.position .- e[Spring].center) .* e.spring_constant
        e[Spatial] = Spatial(e.position, new_v)
    end
end

function update_new(::Rotator, m::AbstractLedger)
    dt = 0.01
    @inbounds for e in @entities_in(m, Rotation && Spatial) 
        n          = e.axis
        r          = e.position .- e.center 
        theta      = e.omega * dt
        nnd        = n .* sum(n .* r)
        t = (r[2] * n[3] - r[3] * n[2], r[3]*n[1] - r[1] * n[3] , r[1] * n[2] - r[2] * n[1])
        e[Spatial] = Spatial(e.center .+ nnd .+ (r .- nnd) .* cos(theta) .+ t .* sin(theta), e.velocity)
    end
end

function update_new(::Mover, m::AbstractLedger)
    dt = 0.01
    @inbounds for e in @entities_in(m, Spatial)
        e[Spatial] = Spatial(e.position .+ e.velocity.*dt, e.velocity)
    end
end

suite["real life"]["update"]["new school empty"] = @benchmarkable (update_new(Oscillator(), m);
                                                      update_new(Rotator(), m);
                                                      update_new(Mover(), m)) setup=(m = Ledger(st))
suite["real life"]["update"]["new school full"] = @benchmarkable (update_new(Oscillator(), m);
                                                      update_new(Rotator(), m);
                                                      update_new(Mover(), m)) setup=(m = Ledger(st); e_fill(m))

end
BenchComponent.suite
