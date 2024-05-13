//I_ZDDJ: There is no syntax for an integer with an empty constraint,
//although parameters of subprograms can declare under-constrined integers.

// RUN: interp %s | FileCheck %s

func under_constrained(a: integer{})
begin
    pass;
end

func main() => integer
begin
    under_constrained(10);
    return 0;
end
