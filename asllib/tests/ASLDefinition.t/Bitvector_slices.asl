func main() => integer
begin
    let bv : bits(6) = '110010';
    assert  bv[5] == '1' &&
            bv[4] == '1' &&
            bv[3] == '0' &&
            bv[2] == '0' &&
            bv[1] == '1' &&
            bv[0] == '0';
    assert bv == bv[5,4,3,2,1,0];
    assert bv != bv[0,1,2,3,4,5];
    assert bv == bv[5:0];
    assert bv == bv[:6];
    assert bv[3:0] == bv[:4];
    assert bv == bv[5:5] :: bv[4:4] :: bv[3:3] :: bv[2:2] :: bv[1:1] :: bv[0:0];

    // Formally, [j:i] is interpreted as a slice of bits
    // from index i of length (j-i)+1.
    // So the slice below starts at index 0 and has length (-1-0)+1 = 0.
    // That is, an empty slice.
    assert bv[-1:0] == '';
    return 0;
end;
