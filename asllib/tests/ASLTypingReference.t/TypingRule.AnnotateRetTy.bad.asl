func flip{N}(x: bits(N)) => bits(N)
begin
    return x XOR Ones{N};
end;

func proc()
begin
    pass;
end;

func main() => integer
begin
    var bv = Zeros{64};
    - = flip{64}(bv);
    flip{64}(bv); // Illegal: the returned value must be consumed.
    - = proc(); // Illegal: `proc` does not return a value.
    return 0;
end;
