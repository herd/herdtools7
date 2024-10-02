////R_GHXR: The remainder operation frem_int is defined as the remainder of
//division rounding towards negative infinity. The second operand must be a
//positive integer.

// RUN: not interp %s | FileCheck %s

func main() => integer
begin
    print(frem_int(6, -3));
    return 0;
end
