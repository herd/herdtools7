//R_ZYWY: If both operands of an integer binary primitive operator are
//integers and at least one of them is an unconstrained integer then the
//result shall be an unconstrained integer.

// RUN: interp %s | FileCheck %s

func main() => integer
begin
    var a: integer = 10;
    var b: integer = a + 10;

    return 0;
end
