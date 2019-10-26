struct Entity
    id::Int
end

function Entity(m::AbstractManager)
	if !isempty(free_entities(m))
		e = pop!(free_entities(m))
		entities(m)[e.id] = e
		return e
	end
	n = length(entities(m)) + 1
	e = Entity(n)
	push!(entities(m), e)
	return e
end

function Entity(m::AbstractManager, datas::ComponentData...)
	e = Entity(m)
	for d in datas
		m[e] = d
	end
	return e
end

function Entity(c::AbstractComponent, i::Integer)
    return Entity(c.indices.packed[i])
end

const EMPTY_ENTITY = Entity(0)
