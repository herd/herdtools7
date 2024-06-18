//R_GHXR: The remainder operation frem_int is defined as the remainder of 
//division rounding towards negative infinity. The second operand must be a
//positive integer.

// RUN: interp %s | FileCheck %s

func main() => integer
begin
    assert ( 6 MOD 3) == 0;
    assert (-5 MOD 3) == 1;
    return 0;
end
