func xor_extend{N, M}(x: bits(N), y: bits(M)) => bits(N)
begin
    return x XOR ZeroExtend{N, M}(y);
end;

func main() => integer
begin
    var bv1 = Zeros{64};
    var bv2 = Zeros{32};
    // Legal: all parameters and arguments are given.
    - = xor_extend{64, 32}(bv1, bv2);
    // Illegal: both parameters are given, but missing argument for `y`.
    - = xor_extend{64, 32}(bv1);
    return 0;
end;
