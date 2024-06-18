//R_FHYZ: If at least one argument is an under-constrained width bitvector
//then the result of the bitvector concatenation is an under-constrained
//width bitvector.

// RUN: interp %s | FileCheck %s

func test{N}(a: bits(N))
begin
    var b = [a, '11'];
end

func main() => integer
begin
    return 0;
end
