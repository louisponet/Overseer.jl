using BenchmarkTools
using Parameters

@component @with_kw struct Spatial
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


suite = BenchmarkGroup()
suite["creation"] = BenchmarkGroup()

st = Stage(:simulation, [Oscillator(), Rotator(), Mover()])

suite["creation"]["ledger"] = @benchmarkable m = Ledger($st)
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

suite["creation"]["filling entities"] = @benchmarkable e_fill(m) setup=(m = Ledger(st))

suite["update"] = BenchmarkGroup()

suite["update"]["old school empty"] = @benchmarkable update(m) setup=(m = Ledger(st))
suite["update"]["old school full"] = @benchmarkable update(m) setup=(m = Ledger(st); e_fill(m))

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

suite["update"]["new school empty"] = @benchmarkable (update_new(Oscillator(), m);
                                                      update_new(Rotator(), m);
                                                      update_new(Mover(), m)) setup=(m = Ledger(st))
suite["update"]["new school full"] = @benchmarkable (update_new(Oscillator(), m);
                                                      update_new(Rotator(), m);
                                                      update_new(Mover(), m)) setup=(m = Ledger(st); e_fill(m))

results = run(suite)

@show results

