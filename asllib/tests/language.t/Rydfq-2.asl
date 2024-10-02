//R_YDFQ: Where the left hand side of an assignment is to an identifier 
//which is declared as a setter, the assignment is treated in the same way
//as a procedure invocation. In this case, if a sequence of
//null_or_slice_lists is present, each must consist of a single expression.
//The sequence of null_or_slice_lists shall be used as the actual
//expressions for the invocation of the setter.

// RUN: interp %s | FileCheck %s
// CHECK: 4

var _a: integer = 10;

getter a => integer
begin
    return _a;
end

setter a = value: integer
begin
    _a = value;
end

func main() => integer
begin
    a = 4;
    print(_a);

    return 0;
end

// XFAIL: *