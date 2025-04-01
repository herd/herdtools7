func plus{N}(x: bits(N + 2), z: integer{0..N}) => bits(N + 2)
begin
    return x + z;
end;

func main() => integer
begin
    var bv1 = Zeros{64};
    let z = 40;
    - = plus{z + 22}(bv1, z);
    return 0;
end;
