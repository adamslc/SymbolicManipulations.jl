function count_ops(x::SymbolicUtils.Mul)
    ops = x.coeff == 1 ? 0 : 1
    for (term, pow) in pairs(x.dict)
        ops += 1 + count_ops(term) + (pow == 1 ? 0 : 1)
    end
    return ops - 1 # The for loop overcounts operations by one
end
function count_ops(x::SymbolicUtils.Add)
    ops = x.coeff == 0 ? 0 : 1
    for (term, mult) in pairs(x.dict)
        ops += 1 + count_ops(term) + (mult == 1 ? 0 : 1)
    end
    return ops - 1 # The for loop overcounts operations by one
end
function count_ops(x::SymbolicUtils.Term)
    ops = 1
    for a in x.arguments
        ops += count_ops(a)
    end
    return ops
end
count_ops(x::SymbolicUtils.Pow) = 1 + count_ops(x.base) + count_ops(x.exp)
count_ops(::SymbolicUtils.Sym) = 0
count_ops(::Int) = 0


struct TrialRewriter
    trial_rewrites
    simplify_rewrite
    complexity_measure

    function TrialRewriter(trial_rewrites, simplify_rewrite,
            complexity_measure=count_ops)

        return new(trial_rewrites, simplify_rewrite, complexity_measure)
    end
end

function (tr::TrialRewriter)(x)
    complexity = tr.complexity_measure(x)
    for trial_rewrite in tr.trial_rewrites
        trial_walk = SymbolicUtils.Postwalk(SymbolicUtils.PassThrough(trial_rewrite))
        y = tr.simplify_rewrite(trial_walk(x))

        println("  ", x, "  ==>  ", y)

        # y shouldn't ever be able to be nothing, but might as well check.
        if y !== nothing
            new_complexity = tr.complexity_measure(y)
            if new_complexity < complexity
                complexity = new_complexity
                x = y
            end
        end
    end
    return x
end

full_simplify_trial_rewrites = [
    # Double angle formulas
    @rule(sin(2 * ~x) => 2 * sin(~x) * cos(~x))
    @rule(cos(2 * ~x) => cos(~x)^2 + -1*sin(~x)^2)
    @rule(tan(2 * ~x) => 2*tan(~x) / (1 + -1*tan(x)^2))

    # TODO: Half angle formulas. Somewhat tricky because there are branches depending
    # on the quadrant that the angle occupies.

    # Trig sum formulas
    # Difference formulas should be derivable using these rules and other basic
    # trig rules.
    @rule(sin(~x + ~y) => sin(~x)*cos(~y) + cos(~x)*sin(~y))
    @rule(cos(~x + ~y) => cos(~x)*cos(~y) + -1*sin(~x)*sin(~y))
    @rule(tan(~x + ~y) => (tan(~x) + tan(~y)) / (1 - tan(~x)*tan(~y)))

    # Hyperbolic trig functions
    @acrule(exp(~x) +  1 => 2 * exp(1//2 * ~x) * cosh(1//2 * ~x))
    @acrule(exp(~x) + -1 => 2 * exp(1//2 * ~x) * sinh(1//2 * ~x))
]

extra_simplify_rules = [
    # Exponential rules
    @rule(exp(~x)^(~y) => exp(~x * ~y))
    @acrule(exp(~x)*exp(~y) => exp(~x + ~y))

    # Trig rules
    @rule(cos(-1*~x) => cos(~x))
    @rule(sin(-1*~x) => -1*sin(~x))
    @rule(tan(-1*~x) => -1*tan(~x))

]
extra_rewritter =
    SymbolicUtils.Postwalk(
    SymbolicUtils.PassThrough(
    SymbolicUtils.Chain(extra_simplify_rules)))

# simplify_rewritter = SymbolicUtils.PassThrough(SymbolicUtils.Empty())
simplify_rewritter =
    SymbolicUtils.Fixpoint(
    SymbolicUtils.Chain((extra_rewritter, SymbolicUtils.serial_simplifier)))

full_simplify_rewriter =
    TrialRewriter(
        full_simplify_trial_rewrites,
        simplify_rewritter)

fullsimplify(ex) = simplify(ex, rewriter=full_simplify_rewriter)
