func parameterized_func{wid: integer}() => bits(wid)
begin
    var ans: bits(wid);
    ans = Zeros{wid};
    return (NOT ans);
end;

func illegal_fun_unconstrained_actual(arg: integer)
begin
    // Illegal invocation of `parameterized_func` since
    // arg is an unconstrained integer whereas parameters need to be constrained
    // integer types.
    - = parameterized_func{arg}();
end;
