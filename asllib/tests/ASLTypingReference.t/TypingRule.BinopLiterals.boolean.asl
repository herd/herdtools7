func main() => integer
begin
    println("and_bool: TRUE && TRUE = ", TRUE && TRUE);
    println("and_bool: TRUE && FALSE = ", TRUE && FALSE);
    println("or_bool: TRUE || FALSE = ", TRUE || FALSE);
    println("or_bool: FALSE || FALSE = ", FALSE || FALSE);
    println("eq_bool: FALSE == FALSE = ", FALSE == FALSE);
    println("eq_bool: TRUE == TRUE = ", TRUE == TRUE);
    println("eq_bool: FALSE == TRUE = ", FALSE == TRUE);
    println("ne_bool: FALSE != FALSE = ", FALSE != FALSE);
    println("ne_bool: TRUE != TRUE = ", TRUE != TRUE);
    println("ne_bool: FALSE != TRUE = ", FALSE != TRUE);
    println("implies_bool: FALSE ==> FALSE = ", FALSE ==> FALSE);
    println("implies_bool: FALSE ==> TRUE = ", FALSE ==> TRUE);
    println("implies_bool: TRUE ==> TRUE = ", TRUE ==> TRUE);
    println("implies_bool: TRUE ==> FALSE = ", TRUE ==> FALSE);
    println("equiv_bool: FALSE <=> FALSE = ", FALSE <=> FALSE);
    println("equiv_bool: TRUE <=> TRUE = ", TRUE <=> TRUE);
    println("equiv_bool: FALSE <=> TRUE = ", FALSE <=> TRUE);
    // println("invalid", TRUE == 1);
    // println("invalid", FALSE && 0);
    return 0;
end;
