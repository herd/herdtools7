//R_NYNK: If any argument of a bitvector concatenation has undetermined
//width then the result of the bitvector concatenation has undetermined
//width.

// RUN: interp %s | FileCheck %s

func test{N: integer}(a: bits(N))
begin
    var b = [a, '11'];
end

func main() => integer
begin
    return 0;
end
