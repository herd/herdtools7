constant GLOBAL_CONSTANT = 5;
var global_non_constant = 19;

func main() => integer
begin
    let effective_local_constant = 7;
    var var_x = effective_local_constant;
    var y = var_x;
    var local_non_constant = effective_local_constant;
    var z = local_non_constant + GLOBAL_CONSTANT + global_non_constant;
    return 0;
end;
