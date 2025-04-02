func xor_extend{N, M}(x: bits(N), y: bits(M)) => bits(N)
begin
    return x XOR ZeroExtend{N, M}(y);
end;

func main() => integer
begin
    var bv1 = Zeros{64};
    var bv2 = Zeros{32};
    - = xor_extend{64, 32}(bv1, bv2);
    - = xor_extend{64}(bv1, bv2); // Illegal: missing parameter for `M`.
    return 0;
end;
