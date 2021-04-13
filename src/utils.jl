# Checks if an expression contains variable var
is_const(ex::SymbolicUtils.Pow, var::SymbolicUtils.Sym) = is_const(ex.base, var) && is_const(ex.exp, var)
is_const(ex::SymbolicUtils.Mul, var::SymbolicUtils.Sym) = all(is_const.(keys(ex.dict), Ref(var)))
is_const(ex::SymbolicUtils.Add, var::SymbolicUtils.Sym) = all(is_const.(keys(ex.dict), Ref(var)))
is_const(ex::SymbolicUtils.Sym, var::SymbolicUtils.Sym) = ex !== var
is_const(::Number, var::SymbolicUtils.Sym) = true

# Checks if an expression is an elementry function in the list of symbols/terms vars
is_elf(ex::SymbolicUtils.Pows) = is_elf(ex.base) & is_elf(ex.exp)
is_elf(ex::SymbolicUtils.Mul) = all(is_elf, keys(ex.dict))
is_elf(ex::SymbolicUtils.Add) = all(is_elf, keys(ex.dict))
is_elf(::SymbolicUtils.Sym) = true
is_elf(::Number) = true
is_elf(ex::SymbolicUtils.Term) = ex.f in [sin, cos, tan, csc, sec, cot, asin, acos, atan, acsc, asec, acot, log, log10]
is_elf(::Any) = false
