func main() => integer
begin
    println "add_bits: '010' + '011' = ", '010' + '011';
    println "add_bits: '10' + '11' = ", '10' + '11';
    println "add_bits: '010' + 3 = ", '010' + 3;
    println "add_bits: '10' + 3 = ", '10' + 3;
    println "sub_bits: '100' - '010' = ", '100' - '010';
    // println "invalid (different widths)", '100' - '10';
    println "sub_bits: '100' - '111' = ", '100' - '111';
    println "sub_bits: '100' - 7 = ", '100' - 7;
    println "sub_bits: '100' - 8 = ", '100' - 8;
    println "and_bits: '100' AND '111' = ", '100' AND '111';
    println "or_bits: '100' OR '110' = ", '100' OR '110';
    println "xor_bits: '100' XOR '110' = ", '100' XOR '110';
    println "eq_bits: '100' == '110' = ", '100' == '110';
    // println "invalid (different widths)", "'100' == '1100' = ", '100' == '1100';
    println "ne_bits: '100' != '110' = ", '100' != '110';
    println "concat_bits: '100' :: '110' = ", '100' :: '110';
    println "concat_bits: '100' :: '' = ", '100' :: '';
    return 0;
end;
