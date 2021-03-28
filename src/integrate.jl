# Procedure derived from James Slagle's MIT Ph.D. Thesis

struct Integral{I <: SymbolicUtils.Symbolic, T} <: SymbolicUtils.Symbolic{T}
    integrand::I
    variable::SymbolicUtils.Sym
    limits::Union{NTuple{2, T}, Nothing}
    value::Ref{Union{SymbolicUtils.Symbolic, Nothing}}
    equivalent_forms::Vector{SymbolicUtils.Symbolic}
    ef_integrals::Vector{Vector{Integral}}
    parents::Vector{Integral}
    charater
end

isintegral(i::Integral) = true
isintegral(x) = false

issolved(i::Integral) = i.value !== nothing

function isneeded(i::Integral)
    issolved(i) && return false
    length(i.parents) == 0 && return true
    return all(isneeded.(i.parents))
end

function check_solution!(i::Integral)
    for (n, form_integrals) in enumerate(i.ef_integrals)
        solved = all(issolved.(form_integrals))

        r = @rule(~i::isintegral => (~i).value)
        sub = SymbolicUtils.Postwalk(SymbolicUtils.PassThrough(r))

        set_solution!(i, sub(i.equivalent_forms[i]))
    end
end

function set_solution!(i::Integral, solution)
    i.value[] = solution

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

function factor_constant(p)
    typeof(p.ex) <: SymbolicUtils.Mul || return []

    const_term = p.ex.coeff
    variable_term = one(p.ex.coeff)

    for (expr, pow) in pairs(p.ex.dict)
        if is_const(expr, p.v)
            const_term *= expr^pow
        else
            variable_term *= expr^pow
        end
    end

    const_term === one(p.ex.coeff) && return []

    new_int_prob = IntegrationProblem(variable_term, p.v)
    func_prob = FunctionProblem(nothing, sol -> const_term * sol, new_int_prob, [p])
    push!(new_int_prob.parents, func_prob)
    push!(p.subproblems, func_prob)
    return [new_int_prob]
end

function decompose(p)
    typeof(p.ex) <: SymbolicUtils.Add || return []
    length(p.ex.dict) + (p.ex.coeff == 0 ? 0 : 1) > 1 || return []

    sum_prob = SummationProblem(nothing, [], 0, [p])
    push!(p.subproblems, sum_prob)
    if p.ex.coeff != 0
        const_prob = IntegrationProblem(p.ex.coeff, p.v, sum_prob)
        push!(sum_prob.subproblems, const_prob)
    end

    for (expr, coeff) in pairs(p.ex.dict)
        int_prob = IntegrationProblem(coeff * expr, p.v, sum_prob)
        push!(sum_prob.subproblems, int_prob)
    end

    return sum_prob.subproblems
end

function linear_sub(p)
    # TODO: I expect that this section will benefit from CSE. I'll leave it
    # alone for now.
end

# This doesn't quite have the same function as in the paper because it
# recursively expands polynomials, instead of only at the top level. Not sure
# if that's a problem yet...
function expand(p)
    new_ex = SymbolicUtils.polynormalize(p.ex)
    isequal(new_ex, p.ex) && return []

    new_int_prob = IntegrationProblem(new_ex, p.v, p)
    push!(p.subproblems, new_int_prob)
    return [new_int_prob]
end

# Again, I am not strictly following the paper here because simplify can do
# much more than just eliminate common factors between a numerator and
# denominator.
function combine_factors(p)
    new_ex = simplify(p.ex)
    isequal(new_ex, p.ex) && return []

    new_int_prob = IntegrationProblem(new_ex, p.v, p)
    push!(p.subproblems, new_int_prob)
    return [new_int_prob]
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

function add_to_prob_list!(list, prob)
    # Try for a direction solution before adding to the goal list
    new_ex = _apply_standard_forms(prob.ex, prob.v)
    if new_ex !== nothing
        prob.sol = new_ex
        propagate_solution!(prob)
        return
    end

    enqueue!(list, prob)

    # Check for algorithmic transformations
    for algo in algo_transforms
        new_problems = algo(prob)
        add_to_prob_list!.(Ref(list), new_problems)
    end
end

function integrate(ex, v)
    # The temporary goal list holds goals that have not had an immediate
    # solution attempted yet
    temp_prob_list = Queue{AbstractProblem}()
    # If a goal from temp_prob_list does not have an immediate solution, then
    # it's character is computed, and it is examined for possible heuristic
    # transformations. Each possible heuristic transformation is a new goal,
    # and all of these goals are sorted into the heuristic_goal_list so that the
    # goals that are most likely to have a solution are considered first.
    heuristic_prob_list = PriorityQueue{AbstractProblem, Int}()

    # Create a IntegrationProblem object and add it to the goal list
    initial_prob = IntegrationProblem(ex, v)
    add_to_prob_list!(temp_prob_list, initial_prob)

    # TODO: I should track resource usage here too.
    while true
        is_solved(initial_prob) && return initial_prob.sol

        # Move problems from temp_prob_list to heuristic_prob_list

        # If no heuristic problems remain, then we failed to solve the integral
        isempty(heuristic_prob_list) && return nothing

        # Take the cheapest problem from the heuristic_prob_list
        current_goal = dequeue!(heuristic_prob_list)
    end

    return initial_prob.sol
end
