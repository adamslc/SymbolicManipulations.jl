together_rules = [
    # TODO: This rule needs to be defined in three different ways to account for
    # the possible orderings of the numerators and denominators (the middle rule
    # covers two cases because it is associative). It would be nice to replace
    # them all with a single rule.
    @acrule(~a * (~b)^-1 + ~c * (~d)^-1 => (~a*~d + ~b*~c) / (~b*~d))
    @acrule((~b)^-1 * ~a + ~c * (~d)^-1 => (~a*~d + ~b*~c) / (~b*~d))
    @acrule((~b)^-1 * ~a + (~d)^-1 * ~c => (~a*~d + ~b*~c) / (~b*~d))
]
together_rewriter =
    SymbolicUtils.Postwalk(
        SymbolicUtils.PassThrough(
            SymbolicUtils.Chain(together_rules)
        )
    )

together(ex; kwargs...) = simplify(ex; rewriter=together_rewriter, kwargs...)
