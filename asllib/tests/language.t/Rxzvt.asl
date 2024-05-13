//R_XZVT: The type of a conditional expression is the lowest common ancestor
//of the types of the then and else expressions.

// RUN: interp %s | FileCheck %s

config test = TRUE;
func main() => integer
begin
    var a: integer{0, 1} = if test then 0 else 1;
    return 0;
end
