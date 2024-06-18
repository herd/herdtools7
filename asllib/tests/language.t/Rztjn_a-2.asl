//R_ZTJN: The operation div_int performs exact division. The divisor (the 
//second operand) must be a positive integer that exactly divides the first
//operand.

// RUN: interp %s | FileCheck %s

func main() => integer
begin
    assert (6  DIV 3 ) == 2;
    assert (-6 DIV  3) == -2;
    return 0;
end
