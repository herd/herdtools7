//I_ZDDJ: There is no syntax for an integer with an empty constraint, 
//although parameters of subprograms can declare under-constrined integers.

// RUN: interp %s | FileCheck %s

func under_constrained(a: integer, b: bits(a))
begin
    pass;
end

func main() => integer
begin
    under_constrained(10, '1111 1111 11');
    return 0;
end
