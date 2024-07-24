//I_RKBV: Unconstrained integer parameters are treated as under-constrained 
//integer parameters both within the subprogram and at invocations.

// RUN: interp %s | FileCheck %s

func test(a: integer)
begin
    pass;
end

func test2{a}(b: bits(a))
begin
    test(a);
end

func main() => integer
begin
    return 0;
end
