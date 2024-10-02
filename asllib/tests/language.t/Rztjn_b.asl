//R_ZTJN: The operation div_int performs exact division. The divisor (the 
//second operand) must be a positive integer that exactly divides the first
//operand.

// RUN: not interp %s | FileCheck %s

func main() => integer
begin
    print(div_int(6, 2));
    return 0;
end
