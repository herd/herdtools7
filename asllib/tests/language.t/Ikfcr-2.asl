//I_KFCR: It follows from the domain rules that under-constrained integers 
//type-satisfy unconstrained integers but not vice versa so an unconstrained
//integer may not be used where a constrained integer is required by uses of
//the type-satisfaction rule, even if the constrained integer is
//under-constrained.

// RUN: interp %s | FileCheck %s

func test{N}(a: bits(N))
begin
    var b: integer = N;
    return;
end

func main() => integer
begin
    constant a: integer{0..10} = 1;
    var b: bits(a);
    test(b);
    return 0;
end
