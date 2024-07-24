// RUN: interp %s | FileCheck %s

func testParams{K : integer} (N: integer, M: integer, L: integer, lbv: bits(L), kbv: bits(K)) => bits(N)
begin
    var kBits: bits(K); // legal
    // K is a parameter, so it is an under-constrained integer
    var nBits: bits(N); // legal
    // N is a parameter because it is a formal argument and is used in the type of the bitvector of a return type, so it is an under-constrained integer
    var lBits: bits(L); // legal
    // L is a parameter because it is a formal argument and is used in the type of the bitvector of a formal argument, so it is an under-constrained integer
    // var mBits: bits(M); // ILLEGAL
    // M is not a parameter so it is an unconstrained integer
    return nBits;
end

func main() => integer
begin
    return 0;
end
