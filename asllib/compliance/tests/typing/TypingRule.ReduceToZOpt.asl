constant A = 2;

func main() => integer
begin
    // since `A * 2 + 3` can be normalized into 7,
    // the base value inferred for `bv1` is '0000000'.
    var bv1: bits(A * 2 + 3);
    let B = ARBITRARY: integer{0..10};
    // Since `B` cannot be normalized into a constant
    // integer, the base value inferred for `bv2` is `0[0+:B]`.
    var bv2: bits(B);
    return 0;
end;
