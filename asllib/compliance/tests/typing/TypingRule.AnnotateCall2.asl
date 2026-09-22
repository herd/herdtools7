func parameterized_func{wid: integer}() => bits(wid)
begin
    var ans: bits(wid);
    ans = Zeros{wid};
    return (NOT ans);
end;

// Three cases for invocation of function `parameterized_func`
// which has a parameter
func legal_fun_constrained_actual{N: integer {1,2}}() => bits(N)
begin
    // This invocation is OK because integer{1,2} type-satisfies
    // the type of the wid parameter - integer.
    // The type of the result is bits(N).
    return parameterized_func{N}();
end;

func legal_fun_parameterized_actual{N: integer}() => bits(N)
begin
    return parameterized_func{N}();
end;

func constrained_parameter{N: integer {1,2,3}}() => bits(N)
begin
    return Zeros{N}();
end;

// Cases for invocation of function
// which has a well-constrained integer parameter
func legal_fun_constrained_actual2{N: integer {1,2}}() => bits(N)
begin
    // This invocation is OK because {1,2} is a subset of {1,2,3}
    // The result type is bits(N).
    return constrained_parameter{N}();
end;
