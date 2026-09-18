func main() => integer
begin
    println "mul_int_real: 10 * 0.5 = ", 10 * 0.5;
    println "mul_real_int: 0.5 * 10 = ", 0.5 * 10;
    println "add_real: 10.0 + 0.5 = ", 10.0 + 0.5;
    println "sub_real: 10.0 - 0.5 = ", 10.0 - 0.5;
    println "mul_real: 10.0 * 0.5 = ", 10.0 * 0.5;
    // Invalid as the second argument should be an integer
    // println "invalid", "exp_real: 10.0 ^ 0.5 = ", 10.0 ^ 0.5;
    println "exp_real: 10.0 ^ 2 = ", 10.0 ^ 2;
    // Invalid as '0.0 ^ q' requires 'q' to be non-negative.
    // println "invalid", "exp_real: 0.0 ^ -9 = ", 0.0 ^ -9;
    println "div_real: 10.0 / 0.5 = ", 10.0 / 0.5;
    // Invalid as the second argument should not be 0.0
    // println "invalid", "div_real: 10.0 / 0.0 = ", 10.0 / 0.0;
    println "eq_real: 10.0 == 0.5 = ", 10.0 == 0.5;
    println "ne_real: 10.0 != 0.5 = ", 10.0 != 0.5;
    println "le_real: 10.0 <= 0.5 = ", 10.0 <= 0.5;
    println "lt_real: 10.0 < 0.5 = ", 10.0 < 0.5;
    println "gt_real: 10.0 > 0.5 = ", 10.0 > 0.5;
    println "ge_real: 10.0 >= 0.5 = ", 10.0 >= 0.5;
    return 0;
end;
