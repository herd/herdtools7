constant GLOBAL_CONSTANT = 5;
var global_non_constant = 19;

func main() => integer
begin
    constant LOCAL_CONSTANT = 7;
    var var_x = LOCAL_CONSTANT; // The annotated expression for LOCAL_CONSTANT is 7.
    var y = var_x; // The annotated expression for var_x is var_x.
    var local_non_constant = LOCAL_CONSTANT;
    var z = local_non_constant + GLOBAL_CONSTANT + global_non_constant;
    return 0;
end;
