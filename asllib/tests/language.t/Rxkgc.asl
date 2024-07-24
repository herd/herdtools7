//R_XKGC: It is an error for an expression’s meaning to rely on evaluation
//order except that conditional expressions, and uses of the boolean
//operators &&, ||, -->, are guaranteed to evaluate from left to right.

// RUN: not interp %s | FileCheck %s

func main() => integer
begin
    var a: integer = 10 - 5 - 3;
    return 0;
end
