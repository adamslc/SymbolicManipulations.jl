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
macro eqtest_broken(expr)
    @assert expr.head == :call && expr.args[1] in [:(==), :(!=)]
    if expr.args[1] == :(==)
        :(@test_broken isequal($(expr.args[2]), $(expr.args[3])))
    else
        :(@test_broken !isequal($(expr.args[2]), $(expr.args[3])))
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

    @testset "count_ops" begin
        @syms x y z

        @test SymbolicManipulations.count_ops(x) == 0
        @test SymbolicManipulations.count_ops(1) == 0
        @test SymbolicManipulations.count_ops(x + y) == 1
        @test SymbolicManipulations.count_ops(x + y + z) == 2

        @test SymbolicManipulations.count_ops(2x) == 1
        @test SymbolicManipulations.count_ops(2x + 0) == 1
        @test SymbolicManipulations.count_ops(2x + 1) == 2
        @test SymbolicManipulations.count_ops(2x + y + 1) == 3

        @test SymbolicManipulations.count_ops(x^2) == 1
        @test SymbolicManipulations.count_ops(x^2 * y^3) == 3

        @test SymbolicManipulations.count_ops(x^(2y + z)) == 3
    end

    @testset "fullsimplify" begin
        @syms x

        @eqtest fullsimplify(cos(2x) + 2sin(x)^2) == 1
        @eqtest fullsimplify(exp(x) * (exp(x) - 1)^-2) == 1//4*(sinh(1//2 * x))^-2
    end

    @testset "integrate" begin
        @syms x y

        @eqtest integrate(x^2, x) == x^3 // 3
        @eqtest integrate(x^y, x) == x^(y + 1) / (y + 1)
        @eqtest integrate(y^x, y) == y^(x + 1) / (x + 1)

        @eqtest_broken integrate(sin(x)^2 * cos(x), x) == sin(x)^3 // 3
        @eqtest_broken integrate((x^2 + x) / sqrt(x), x) == 2//15 * x^(3//2) * (3x + 5)
        @eqtest_broken integrate(x^4 / (1 - x^2)^(5//2), x) == asin(x) + x//3 * (4x^2 - 3) / (1 - x^2)^(3/2)
        @eqtest_broken integrate(sec(x)^2 / (1 + sec(x)^2 - 3tan(x)), x) == log(2cos(x) - sin(x)) - log(cos(x) - sin(x))
        @eqtest_broken integrate((sin(x) + cos(x))^2, x) == x - 1//2 * cos(2x)
        @eqtest_broken integrate(tan(x) * sec(x)^2, x) == sec(x)^2 // 2
        @eqtest_broken integrate(sin(x) * cos(x), x) == -1//2 * cos(x)^2

        # SAINT could not solve these:
        @eqtest_broken integrate(x * sqrt(1 + x), x) == 2//15 * (1 + x)^(3//2) * (3x - 2)
        @eqtest_broken integrate(cos(sqrt(x)), x) == 2cos(sqrt(x)) + 2sqrt(x)*sin(sqrt(x))
    end
end
