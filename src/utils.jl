"""
Like `filter()[1]`.
"""
function getfirst(f::Function, A)
    for el in A
        if f(el)
            return el
        end
    end
end

"""
	separate!(f::Function, B::T, A::T) where T

Like `separate` but a vector `B` can be given in which the result will be stored.
"""
function separate!(f::Function, B::T, A::T) where T
    nt, nf = 0, length(A)+1
    @inbounds for a in A
        if f(a)
            B[nt+=1] = a
        else
            B[nf-=1] = a
        end
    end
    fid = 1+nt
    reverse!(@view B[fid:end])
	return B, fid
end

"""
	separate(f::Function, A::AbstractVector{T})

Returns an `array` and `index` where `array[1:index-1]` holds all the values of `A` for which `f` returns `true`, and `array[index:end]` holds those for which `f` returns `false`.
"""
separate(f::Function, A::AbstractVector) =
	separate!(f, similar(A), A)


