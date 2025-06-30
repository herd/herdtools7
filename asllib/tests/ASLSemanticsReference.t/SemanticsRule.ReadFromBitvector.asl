func main() => integer
begin
    var bv: bits(32) = '1100 0011 1010 0101 0000 1111 0110 1001';
    var i = 3282374505; // The bitvector literal above as a decimal.
    var empty_bv_slice = bv[0+:0];
    var empty_i_slice = i[0+:0];
    println "empty_bv_slice = ", empty_bv_slice, ", empty_i_slice = ", empty_i_slice;
    var slice_bv = bv[0+: 4, 28+: 4, 4*:4];
    assert slice_bv == '1001 1100 0101';
    var slice_i = i[0+: 4, 28+: 4, 4*:4];
    println "slice_bv = ", slice_bv, ", slice_i = ", slice_i;
    return 0;
end;
