func FlipSlice{M: integer{0..64}, N}(x: bits(N)) => bits(M)
begin
    return x[M-1:0] XOR Ones{M};
end;

constant EIGHT = 8;

func main() => integer
begin
    let bv = '1001 0011';
    let FOUR = 4;
    let bv_flipped = FlipSlice{FOUR, EIGHT}(bv);
    assert bv_flipped == '1100';
    return 0;
end;
