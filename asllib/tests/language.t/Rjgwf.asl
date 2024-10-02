//R_JGWF: An expression which invokes a primitive operator matches an
//operation if the operation implements the operator and the operands of the
//expression type-satisfy the corresponding operands of the operation as
//shown in the Operator definitions.

// RUN: interp %s | FileCheck %s

type a of boolean;

func main() => integer
begin
    var aa: a;
    var aaa: a;

    var b = aa || aaa;
    return 0;
end
