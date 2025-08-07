type Color of enumeration {RED, GREEN, BLUE};

func main() => integer
begin
    println "eq_string: \"hello\" == \"world\" = ", "hello" == "world";
    println "eq_string: \"hello\" == \"hello\" = ", "hello" == "hello";
    println "ne_string: \"hello\" != \"world\" = ", "hello" != "world";
    println "eq_enum: RED == RED = ", RED == RED;
    println "eq_enum: RED == GREEN = ", RED == GREEN;
    println "eq_enum: RED != RED = ", RED != RED;
    println "eq_enum: RED != GREEN = ", RED != GREEN;
    println "concat_string: 0 ++ '1' ++ 2.0 ++ TRUE ++ \"foo\" ++ RED = ",
            0 ++ '1' ++ 2.0 ++ TRUE ++ "foo" ++ RED;
    return 0;
end;
