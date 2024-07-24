//D_BVGK: A constrained width bitvector whose width is given by
//an expression of type ty that has the structure of an
//under-constrained integer is an under-constrained width
//bitvector. Under-constrained width bitvectors have a
//determined width.
// RUN: interp %s | FileCheck %s

func underconstrained(N: integer) => bits(N)
begin
    return Zeros(N);
end

func main() => integer
begin
    return 0;
end
