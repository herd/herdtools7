func main() => integer
begin
    println "negate_int: -10 = ", -10;
    println "negate_int: -0x0 = ", -0x0;
    println "negate_int: -0xf = ", -0xf;
    println "negate_rel: -2.3 = ", -2.3;
    println "not_bool: !TRUE = ", !TRUE;
    // println "invalid", NOT TRUE;
    println "not_bits: NOT '' = ", NOT '';
    println "not_bits: NOT '11 01' = ", NOT '11 01';
    // println "invalid", ! '11 01';
    return 0;
end;
