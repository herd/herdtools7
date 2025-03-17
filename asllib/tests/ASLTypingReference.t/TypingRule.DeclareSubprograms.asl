func flip_bits{N}(bv: bits(N)) => bits(N)
begin
    return Ones{N} XOR bv;
end;

func add_10(x: integer) => integer
begin
    return x + 10;
end;

func add_10(x: real) => real
begin
    return x + 10.0;
end;

func factorial(x: integer) => integer recurselimit 100
begin
    return if x == 0 then 1 else x * factorial(x - 1);
end;
