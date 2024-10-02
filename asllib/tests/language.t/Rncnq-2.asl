//R_NCNQ: If expr is of type ty which has the structure of the
//under-constrained integer then bits(expr) is an under- constrained width
//bitvector and its determined width is expr.

// RUN: interp %s | FileCheck %s

func a(n: integer) => bits(n)
begin
    var b: bits(n);
    return b;
end

func main() => integer
begin
    return 0;
end
