type Color of enumeration {RED, GREEN, BLUE};

func main() => integer
begin
    println("eq_string: \"hello\" == \"world\" = ", "hello" == "world");
    println("eq_string: \"hello\" == \"hello\" = ", "hello" == "hello");
    println("ne_string: \"hello\" != \"world\" = ", "hello" != "world");
    println("eq_enum: RED == RED = ", RED == RED);
    println("eq_enum: RED == GREEN = ", RED == GREEN);
    println("eq_enum: RED != RED = ", RED != RED);
    println("eq_enum: RED != GREEN = ", RED != GREEN);
    return 0;
end;
