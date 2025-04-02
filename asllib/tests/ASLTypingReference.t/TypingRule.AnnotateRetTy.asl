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
    bv = flip{64}(bv);
    proc();
    return 0;
end;
