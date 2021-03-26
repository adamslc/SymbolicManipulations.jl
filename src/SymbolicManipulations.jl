module SymbolicManipulations

using SymbolicUtils
using Setfield

export together, fullsimplify, integrate

include("together.jl")
include("fullsimplify.jl")
include("integrate.jl")

end
