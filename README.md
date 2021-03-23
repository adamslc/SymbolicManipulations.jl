# SymbolicManipulations

[![Build Status](https://github.com/adamslc/SymbolicManipulations.jl/workflows/CI/badge.svg)](https://github.com/adamslc/SymbolicManipulations.jl/actions)

Some utilities for manipulating symbolic expressions created by
`SymbolicUtils.jl`.

Implemented functionality:
* `together`
* `fullsimplify`

The `fullsimplify` routine is powered by a new rewriter called
`TrialRewriter`. The idea is that, to achieve some simplifications, things need
to get worse before they get better. The `TrialRewriter` has a list of candidate
rewrites that it performs on the input expression. After each candidate rewrite,
the expression is simplified, and then compared to the original. If the
expression has become simpler (as measured by a heuristic for the complexity of
the expression), then the candidate rewrite is accepted. Otherwise it is
rejected. This process is obviously more computationally expensive then
`SymbolicUtils.simplify`, but I think that it is very often the case that the
user is willing to wait quite a long time---perhaps up to a minute---if the
simplification routine is sufficiently good.

Planned functionality:
* `apart`
* `expand`
* `trigexpand`
* `factor`
