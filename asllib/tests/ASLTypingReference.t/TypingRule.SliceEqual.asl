func foo{N}(bv: bits(N))
begin
    var x : bits(N DIV 2 + 1) = bv[N DIV 2:0];
end;

func main() => integer
begin
    var bv: bits(64);
    bv[5] = bv[5 +: 1];
    bv[3 *: 4] = bv[(3 * 4 + 4) - 1 : 3 * 4];
    // The next statement in comment is illegal as the current equivalence test
    // is too conservative to establish that both slices are equivalent.
    // bv[(3 * 4 + 4) - 1 : 3 * 4] = bv[3 *: 4];
    bv[3 *: 4] = bv[15:12];
    bv[15 : 12] = bv[3 *: 4];
    return 0;
end;
