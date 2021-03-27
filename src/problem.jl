abstract type AbstractProblem end

function propagate_solution!(prob::AbstractProblem)
    for parent in prob.parents
        check_solution!(parent, prob)
    end
end

is_solved(p::AbstractProblem) = p.sol !== nothing

function is_solved_or_unneeded(p::AbstractProblem)
    is_solved(p) && return true
    length(p.parents) == 0 && return false
    return all(is_solved_or_unneeded.(p.parents))
end

# The solution to an IntegrationProblem is the integral indefinite of ex with
# respect to v. This can either be solve directly or by solving any one of its
# subproblems
mutable struct IntegrationProblem <: AbstractProblem
    ex              # The expression to be integrated
    v               # The variable of integration
    sol             # The solution to the integral
    subproblems     # A vector of other problems, any one of which is equivalent
                    # to solving the original problem
    parents         # A vector of other problems that might be solved by solving
                    # this problem
    character       # A tuple of information about the problem, used in deciding
                    # which heuristic transformations to apply
end

function IntegrationProblem(ex, v)
    return IntegrationProblem(ex, v, nothing, [], [], nothing)
end

function IntegrationProblem(ex, v, parent)
    return IntegrationProblem(ex, v, nothing, [], [parent], nothing)
end

function check_solution!(prob::IntegrationProblem, subproblem)
    prob.sol = subproblem.sol
    propagate_solution!(prob)
end


# The solution to a SummationProblem  is the sum of the solutions of each
# subproblem. Thus each subproblem must be solved individually to solve the
# SummationProblem
mutable struct SummationProblem <: AbstractProblem
    sol
    subproblems
    num_solved
    parents
end

function check_solution!(prob::SummationProblem, subproblem)
    prob.num_solved += 1

    if prob.num_solved == length(prob.subproblems)
        prob.sol = prob.subproblems[1].sol

        for i in 2:length(prob.subproblems)
            prob.sol += prob.subproblems[i].sol
        end

        propagate_solution!(prob)
    end
end

# The solution of a FunctionProblem is the specified function applied to the
# correct solution of its subproblem.
mutable struct FunctionProblem <: AbstractProblem
    sol
    f
    subproblem
    parents
end

function check_solution!(prob::FunctionProblem, subproblem)
    prob.sol = prob.f(subproblem.sol)
    propagate_solution!(prob)
end
