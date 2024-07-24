//R_BZKW: If both operands of an integer binary primitive operator are 
//constrained integers and at least one of them is the under-constrained 
//integer then the result shall be an under-constrained integer.

// RUN: interp %s | FileCheck %s

func test(a: integer{}, b: integer{0..3})
begin
    var c = a + b;
end

func main() => integer
begin
    return 0;
end
