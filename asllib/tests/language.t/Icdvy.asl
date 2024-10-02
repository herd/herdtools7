//I_CDVY: It follows from the domain rules that well-constrained integers
//type satisfy under-constrained integers, so a well-constrained integer may
//be used wherever an under-constrained integer is required by uses of the
//type-satisfaction rule, for example, a well-constrained integer may be an
//actual argument when the formal argument is an under-constrained integer.

// RUN: interp %s | FileCheck %s

func test{N}(a: bits(N))
begin
    return;
end

func main() => integer
begin
    constant a: integer{0..10} = 1;
    var b: bits(a);
    test(b);
    return 0;
end
