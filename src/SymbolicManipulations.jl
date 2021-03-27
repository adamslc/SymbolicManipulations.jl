module SymbolicManipulations

using SymbolicUtils
using Setfield
using DataStructures

export together, fullsimplify, integrate

include("together.jl")
include("fullsimplify.jl")
include("problem.jl")
include("integrate.jl")

end
