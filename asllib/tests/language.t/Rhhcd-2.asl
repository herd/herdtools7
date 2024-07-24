//R_HHCD: If expr is of type ty which has the structure of the
//under-constrained integer then the array is an under-constrained array
//with number of indices equal to expr.

// RUN: interp %s | FileCheck %s

func test {N} (a: bits(N))
begin
    var b: array[N] of integer;
end

func main() => integer
begin
    return 0;
end
