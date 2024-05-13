//I_BZVB: For most expressions, the width of a bitvector in a formal
//argument or return type will be at least the under-constrained integer.
//For example, a width which is given the sum of two parameters is an
//under-constrained integer.

// RUN: interp %s | FileCheck %s

func test(a: integer, b: integer) => bits(a + b)
begin
    return [Zeros(a), Zeros(b)];
end

func main() => integer
begin
    return 0;
end
