func constrained_func{N: integer {1,2,3}}() => bits(N)
begin
    return Zeros{N}();
end;

func illegal_fun_constrained_actual{N: integer {2,3,4}}() => bits(N)
begin
    // Illegal because {2,3,4} is NOT a subset of {1,2,3}
    return constrained_func{N}();
end;
