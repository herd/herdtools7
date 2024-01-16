// RUN: interp %s | FileCheck %s

var gWid: integer {8,16,32};
// Note that a and b are "formal arguments" but are not "parameters"!
func prod(a: integer, b: integer) => integer
begin
    return (a*b);
end

func declarations()
begin
    let N = gWid;
    // N is an execution-time immutable constrained integer
    // The implicit type of N is integer {8,16,32}
    var A: bits(N);
    // A is a constrained width bitvector of determined width N
    // whose width is constrained to be IN {8,16,32}
    var B: bits(N * 2);
    // B is a constrained width bitvector of determined width N*2
    // whose width is constrained to be IN {16,32,64}
    // The following contrived example shows that even if
    // prod is a trivial function, the result is unconstrained
    let prodWid = prod(N,2); // implicitly an integer
    // var C: bits(prodWid);
    // Illegal since prod(N,2) is an unconstrained integer
    var D: bits(prodWid as {16,32,64});
    // Legal but requires an execution-time check that (prod(N,2) IN {16,32,64})
    // This check may be elided by the compiler
    // D is a constrained width bitvector of determined width prodWid
    // whose width is constrained to be IN {16,32,64}
    var E: bits(N as {64,128}); // Illegal - fails type-check
    // Requires (N IN {64,128})
    // This can be determined as false at compile time since
    // the type of N is integer {8,16,32}.
end

func main() => integer
begin
    return 0;
end
