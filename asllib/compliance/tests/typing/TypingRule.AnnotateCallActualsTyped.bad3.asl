func plus{N}(x: bits(N), z: integer{0..N}) => bits(N)
begin
    return x + z;
end;

func main() => integer
begin
    var bv1 = Zeros{64};
    var z: integer{0..31, 32..64} = 40;
    - = plus{64}(bv1, z);
    var w: integer{0..128};
    // The following statement is illegal as the type of `w`, integer{0..128},
    // does not type-satisfy the type of `z`, integer{0..64}.
    - = plus{64}(bv1, w);
    return 0;
end;
