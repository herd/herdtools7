// The notation <x>--><e> is used to indicate that an occurrence of <x> can be
// rewritten to <e>

func FPZero{N: integer {16,32,64}}(sign: bit) => bits(N)
begin
    // type checker knows N-->N
    let E: integer{} = if N == 16 then 5 else (if N == 32 then 8 else 11);
    // type checker knows E-->if N == 16 then 5 elsif N == 32 then 8 else 11
    let F: integer{} = (N - E) - 1;
    // type checker knows F-->N-(if N == 16 then 5 elsif N == 32 then 8 else 11) - 1
    // which is F-->(if N == 16 then (N -5) elsif N == 32 then ( N-8) else (N-11)) - 1
    // which is F-->(if N == 16 then (16-5) elsif N == 32 then (32-8) else (N-11)) - 1
    // which is F-->(if N == 16 then ( 11) elsif N == 32 then ( 24) else (N-11)) - 1
    // which is F-->(if N == 16 then ( 10) elsif N == 32 then ( 23) else (N-12)) - 1
    var exp = Zeros{E};
    var frac = Zeros{F};
    return sign :: exp :: frac;
    // type checker knows width of return expression is 1 + E + F
    // which is 1
    // + (if N == 16 then 5 elsif N == 32 then 8 else 11 )
    // + (if N == 16 then 10 elsif N == 32 then 23 else (N-12))
    // which is 1
    // + (if N == 16 then 15 elsif N == 32 then 31 else (N- 1))
    // which is
    // (if N == 16 then 16 elsif N == 32 then 32 else N )
    // which is N
end;
