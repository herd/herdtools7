func rotate_right{N}(src: bits(N), amount: integer) => bits(N)
begin
    let i = (amount MOD N) as integer {0..N-1};
    // upper may be a zero width bitvector
    let upper: bits(i) = src[0+:i];
    let lower: bits(N-i) = src[i+:N-i];
    return upper :: lower;
end;

func main() => integer
begin
    var bv = '10100';
    println "bv=", bv, ", rotated twice=", rotate_right{5}(bv, 2);
    return 0;
end;
