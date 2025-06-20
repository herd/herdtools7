func constrained_func{N: integer {1,2,3}}() => bits(N)
begin
    return Zeros{N}();
end;

func illegal_fun_parameterized_actual{N: integer}() => bits(N)
begin
    // Illegal since integer{N} does not type-satisfy integer{1,2,3}.
    return constrained_func{N}(); // requires an asserting type conversion
end;
