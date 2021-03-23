function compute_complexity(m::SymbolicUtils.Mul)
    complexity = 0
    for (term, pow) in pairs(m.dict)
        complexity *= compute_complexity(term)^pow
    end
    return complexity
end
function compute_complexity(a::SymbolicUtils.Add)
    complexity = 0
    for (term, pow) in pairs(a.dict)
        complexity += pow * compute_complexity(term)
    end
    return complexity
end
function compute_complexity(t::SymbolicUtils.Term)
    complexity = 0
    for a in t.arguments
        complexity += compute_complexity(a)
    end
    return 2 * complexity
end
compute_complexity(p::SymbolicUtils.Pow) = compute_complexity(p.base)^compute_complexity(p.exp)
compute_complexity(::SymbolicUtils.Sym) = 1
compute_complexity(::Int) = 1


struct TrialRewriter
    trial_rewrites
    simplify_rewrite
    complexity_measure

    function TrialRewriter(trial_rewrites, simplify_rewrite,
            complexity_measure=compute_complexity)

        return new(trial_rewrites, simplify_rewrite, complexity_measure)
    end
end

function (tr::TrialRewriter)(x)
    complexity = tr.complexity_measure(x)
    for trial_rewrite in tr.trial_rewrites
        trial_walk = SymbolicUtils.Postwalk(SymbolicUtils.PassThrough(trial_rewrite))
        y = tr.simplify_rewrite(trial_walk(x))

        # y shouldn't ever be able to be nothing, but might as well check.
        if y !== nothing
            new_complexity = compute_complexity(y)
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
]

full_simplify_rewriter = TrialRewriter(full_simplify_trial_rewrites, SymbolicUtils.serial_simplifier)

fullsimplify(ex) = simplify(ex, rewriter=full_simplify_rewriter)
