# Procedure derived from James Slagle's MIT Ph.D. Thesis

struct Integral{I <: Union{SymbolicUtils.Symbolic, Number}, T} <: SymbolicUtils.Symbolic{T}
# struct Integral{I <: SymbolicUtils.Symbolic, T} <: SymbolicUtils.Symbolic{T}
    integrand::I
    variable::SymbolicUtils.Sym
    antiderivative::Ref{Union{SymbolicUtils.Symbolic, Nothing}}

    # Not currently used for anything, but will eventually support definite integrals.
    limits::Union{NTuple{2, T}, Nothing}
    value::Ref{Union{SymbolicUtils.Symbolic, Nothing}}

    equivalent_forms::Vector{SymbolicUtils.Symbolic}
    ef_integrals::Vector{Vector{Integral}}
    parents::Vector{Integral}

    charater
end

function Integral(integrand, variable)

    return Integral{typeof(integrand), Float64}(
        integrand,
        variable,
        Ref{Union{SymbolicUtils.Symbolic, Nothing}}(nothing),
        nothing,
        Ref{Union{SymbolicUtils.Symbolic, Nothing}}(nothing),
        SymbolicUtils.Symbolic[],
        Vector{Integral}[],
        Integral[],
        nothing)
end

function isless(a::Integral, b::Integral)
    return isless(a.integrand, b.integrand)
end

function Base.show(io::IO, i::Integral)
    print(io, "Integral(", i.integrand, ", ", i.variable, ")")
end


isintegral(i::Integral) = true
isintegral(x) = false

issolved(i::Integral) = i.antiderivative[] !== nothing

function isneeded(i::Integral)
    issolved(i) && return false
    length(i.parents) == 0 && return true
    return all(isneeded.(i.parents))
end

function check_solution!(i::Integral)
    for (n, form_integrals) in enumerate(i.ef_integrals)
        all(issolved, form_integrals) || continue

        r = @rule(~i::isintegral => (~i).antiderivative[])
        sub = SymbolicUtils.Postwalk(SymbolicUtils.PassThrough(r))

        antiderivative = sub(i.equivalent_forms[n])
        set_solution!(i, antiderivative)
    end
end

function set_solution!(i::Integral, solution)
    i.antiderivative[] = solution

    for parent in i.parents
        check_solution!(parent)
    end

    return solution
end

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
        @rule(v => 1//2 * v^2)
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

function factor_constant(i)
    typeof(i.integrand) <: SymbolicUtils.Mul || return []

    const_term = i.integrand.coeff
    variable_term = one(i.integrand.coeff)

    for (expr, pow) in pairs(i.integrand.dict)
        if is_const(expr, i.variable)
            const_term *= expr^pow
        else
            variable_term *= expr^pow
        end
    end

    const_term === one(i.integrand.coeff) && return []

    new_integral = Integral(variable_term, i.variable)
    push!(i.equivalent_forms, const_term * new_integral)
    push!(i.ef_integrals, [new_integral])
    push!(new_integral.parents, i)

    return [new_integral]
end

function decompose(i)
    typeof(i.integrand) <: SymbolicUtils.Add || return []
    length(i.integrand.dict) + (i.integrand.coeff == 0 ? 0 : 1) > 1 || return []

    integrals = Integral[]
    integral_sum = zero(i.integrand.coeff)
    if i.integrand.coeff != 0
        const_int = Integral(i.integrand.coeff, i.variable)
        push!(const_int.parents, i)
        push!(integrals, const_int)
        integral_sum += const_int
    end

    for (expr, coeff) in pairs(i.integrand.dict)
        int = Integral(coeff * expr, i.variable)
        push!(int.parents, i)
        push!(integrals, int)
        integral_sum += int
    end

    push!(i.equivalent_forms, integral_sum)
    push!(i.ef_integrals, integrals)

    return integrals
end

function linear_sub(p)
    # TODO: I expect that this section will benefit from CSE. I'll leave it
    # alone for now.
end

# This doesn't quite have the same function as in the paper because it
# recursively expands polynomials, instead of only at the top level. Not sure
# if that's a problem yet...
function expand(i)
    new_integrand = SymbolicUtils.polynormalize(i.integrand)
    isequal(new_integrand, i.integrand) && return []

    new_integral = Integral(new_integrand, i.variable)
    push!(new_integral.parents, i)
    push!(i.equivalent_forms, new_integral)
    push!(i.ef_integrals, [new_integral])
    return [new_integral]
end

# Again, I am not strictly following the paper here because simplify can do
# much more than just eliminate common factors between a numerator and
# denominator.
function combine_factors(i)
    new_integrand = simplify(i.integrand)
    isequal(new_integrand, i.integrand) && return []

    new_integral = Integral(new_integrand, i.variable)
    push!(new_integral.parents, i)
    push!(i.equivalent_forms, new_integral)
    push!(i.ef_integrals, [new_integral])
    return [new_integral]
end

function divide_polynomials(p)
    # TODO
end

function half_angle_identities(p)
    # TODO
end

algo_transforms = [
    factor_constant
    decompose
    # linear_sub
    expand
    combine_factors
    # divide_polynomials
    # half_angle_identities
]

function solve_or_add_to_list!(list, integral)
    # Try for a direction solution before adding to the goal list
    new_ex = _apply_standard_forms(integral.integrand, integral.variable)
    if new_ex !== nothing
        set_solution!(integral, new_ex)
        return
    end

    enqueue!(list, integral)

    # Check for algorithmic transformations
    for algo in algo_transforms
        new_integrals = algo(integral)
        solve_or_add_to_list!.(Ref(list), new_integrals)
    end
end

function solve!(initial_integral::Integral)
    temp_list = Queue{Integral}()
    heuristic_list = PriorityQueue{Integral, Int}()

    solve_or_add_to_list!(temp_list, initial_integral)

    # TODO: I should track resource usage here too.
    while true
        issolved(initial_integral) && return true

        # Move problems from temp_list to heuristic_list

        # If no heuristic problems remain, then we failed to solve the integral
        isempty(heuristic_list) && return false

        # Take the cheapest problem from the heuristic_list
        current_goal = dequeue!(heuristic_list)
    end

    return issolved(initial_integral)
end

function integrate(ex, v)
    integral = Integral(ex, v)
    solve!(integral)
    return integral.antiderivative[]
end
