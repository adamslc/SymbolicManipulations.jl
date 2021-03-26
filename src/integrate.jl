# Procedure derived from James Slagle's MIT Ph.D. Thesis

# Unneeded after SymbolicUtils PR is merged and a new version is tagged
import Base: //
//(a::Union{SymbolicUtils.Symbolic, Number}, b::SymbolicUtils.Symbolic) = a / b
//(a::SymbolicUtils.Symbolic, b::T) where {T <: Number} = (one(T) // b) * a

is_const(ex::SymbolicUtils.Pow, var::SymbolicUtils.Sym) = is_const(ex.base, var) && is_const(ex.exp, var)
is_const(ex::SymbolicUtils.Mul, var::SymbolicUtils.Sym) = all(is_const.(keys(ex.dict), Ref(var)))
is_const(ex::SymbolicUtils.Add, var::SymbolicUtils.Sym) = all(is_const.(keys(ex.dict), Ref(var)))
is_const(ex::SymbolicUtils.Sym, var::SymbolicUtils.Sym) = ex !== var
is_const(::Number, var::SymbolicUtils.Sym) = true


function _apply_standard_forms(ex, v)
    is_const_wrt_v(ex) = is_const(ex, v)

    standard_forms = [
        @rule(~c::is_const_wrt_v => ~c*v)

        @rule(exp(v) => exp(v))
        @rule((~c::is_const_wrt_v)^v => ~c^v / log(~c))

        @rule(log(v) => v*log(v) + -1*v)
        @rule(log(~c::is_const_wrt_v, v) => v*log(~c, v) - v / log(~c))

        @rule(sin(v) => -1*cos(v))
        @rule(cos(v) => sin(v))
        @rule(tan(v) => log(sec(v)))
        @rule(cot(v) => log(sin(v)))
        @rule(sec(v) => log(sec(v) + tan(v)))
        @rule(csc(v) => log(csc(v) - cot(v)))

        @rule(asin(v) => v*asin(v) + sqrt(1 - v^2))
        @rule(acos(v) => v*acos(v) - sqrt(1 - v^2))
        @rule(atan(v) => v*atan(v) - 1//2*log(1 + v^2))
        @rule(acot(v) => v*acot(v) + 1//2*log(1 + v^2))
        @rule(asec(v) => v*asec(v) - log(v + sqrt(v^2 - 1)))
        @rule(acsc(v) => v*acsc(v) + log(v + sqrt(v^2 - 1)))

        @rule(sec(v)^2 => tan(v))
        @rule(csc(v)^2 => -1*cot(v))

        @rule(v^-1 => log(v))
        # Need this extra rule because the next one won't match a linear v
        @rule(v => 1//2 * v)
        @rule(v^(~c::is_const_wrt_v) => 1//(~c + 1)*v^(~c + 1))

        @acrule(sec(v)*tan(v) => sec(v))
        @acrule(csc(v)*cot(v) => -1*csc(v))

        # TODO: For the following three rules, I need to ensure that m != +/- n.
        # I don't believe that there is a way to do this using the current rule
        # system.
        # Although, Mathematica doesn't impose this restriction either...
        @acrule(sin(~m*v) * cos(~n*v) => -1//(2*(~m - ~n))*cos((~m - ~n)*v) -
                                          1//(2*(~m + ~n))*cos((~m + ~n)*v))
        @rule(sin(~m*v) * sin(~n*v) => 1//(2*(~m - ~n))*sin((~m - ~n)*v) -
                                       1//(2*(~m + ~n))*sin((~m + ~n)*v))
        @rule(cos(~m*v) * cos(~n*v) => 1//(2*(~m - ~n))*sin((~m - ~n)*v) +
                                       1//(2*(~m + ~n))*sin((~m + ~n)*v))
    ]

    for form in standard_forms
        eval = form(ex)
        eval != nothing && return eval
    end

    return nothing
end

function _factor_constant(ex::SymbolicUtils.Mul, v)
    ex.coeff == 1 && return nothing
    return ex.coeff * integrate(@set ex.coeff = 1, v)
end
_factor_constant(ex, v) = nothing

function _decompose(ex::SymbolicUtils.Add, v)
    int = v*ex.coeff
    for (term, coeff) in pairs(ex.dict)
        new_term = integrate(term, v)
        new_term == nothing && return nothing
        int += coeff * new_term
    end

    return int
end
_decompose(ex, v) = nothing

function _linear_sub(ex, v)
    # TODO: I expect that this section will benefit from CSE. I'll leave it
    # alone for now.
    return nothing
end

# This doesn't quite have the same function as in the paper because it
# recursively expands polynomials, instead of only at the top level. Not sure
# if that's a problem yet...
function _expand(ex, v)
    return integrate(SymbolicUtils.polynormalize(ex), v)
end

# Again, I am not strictly following the paper here because simplify can do
# much more than just eliminate common factors between a numerator and
# denominator.
function _combine_factors(ex, v)
    return integrate(simplify(ex), v)
end

function _divide_polynomials(ex, v)
    # TODO
    return nothing
end

function _half_angle_identities(ex, v)
    # TODO
    return nothing
end

algo_transforms = [
    _factor_constant
    _decompose
    _linear_sub
    _expand
    _combine_factors
    _divide_polynomials
    _half_angle_identities
]

function _algo_transforms(ex, v)
    for algo in algo_transforms
        new_ex = algo(ex, v)
        new_ex !== nothing && return new_ex
    end
    return nothing
end

function integrate(ex, v)
    new_ex = _apply_standard_forms(ex, v)
    new_ex !== nothing && return new_ex

    new_ex = _algo_transforms(ex, v)
    new_ex !== nothing && return new_ex

    return nothing
end
