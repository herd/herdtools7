//R_QCVM: When a subprogram invocation is executed, the actual arguments of
//the invocation are evaluated, and the resulting actual values are used to
//initialize the corresponding identifiers declared in the formal argument
//list. Any bitvector width parameters which are not also formal arguments
//take their value from the width of the related formals. The order of
//evaluation of arguments is unspecified.
 
// RUN: interp %s | FileCheck %s

func test(a: integer, b: integer) => integer
begin
    return 2 * a + b;
end

func main() => integer
begin
    var a = test(10 + 4, test(1, 2));
    return 0;
end
