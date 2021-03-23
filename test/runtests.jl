using SymbolicManipulations
using SymbolicUtils
using Test

# Taken from SymbolicUtils
macro eqtest(expr)
    @assert expr.head == :call && expr.args[1] in [:(==), :(!=)]
    if expr.args[1] == :(==)
        :(@test isequal($(expr.args[2]), $(expr.args[3])))
    else
        :(@test !isequal($(expr.args[2]), $(expr.args[3])))
    end |> esc
end
SymbolicUtils.show_simplified[] = false

@testset "SymbolicManipulations.jl" begin
    @testset "together" begin
        @syms x y z

        @eqtest together(x/z + z/y) == (x*y + z^2) / (y*z)
        @eqtest together(x/z + (1 + z)/y) == (x*y + z*(1 + z)) /(y*z)
        @eqtest together((x + y)/z + (1 + z)/y) == (y*(x + y) + z*(1 + z)) /(y*z)
    end

    @testset "fullsimplify" begin
        @syms x

        @eqtest fullsimplify(cos(2x) + 2sin(x)^2) == 1
    end
end
