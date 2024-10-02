//R_KCZS: If all arguments of a bitvector concatenation have determined
//width then the result of the bitvector concatenation is a determined width
//bitvector whose width is the sum of the arguments’ widths.

// RUN: interp %s | FileCheck %s

func test{N: integer{1..5}}(a: bits(N))
begin
    var b: bits(N+2) = [a, '11'];
end

func main() => integer
begin
    return 0;
end
