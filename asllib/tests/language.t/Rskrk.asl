//R_SKRK: If expr is of type ty which has the structure of a
//well-constrained integer whose domain contains more than one value then
//bits(expr) denotes a constrained width bitvector whose determined width is
//expr and whose width is constrained as per the constraint on expr.

// RUN: interp %s | FileCheck %s
config a: integer{0..10} = 10;
type b of bits(a);

func main() => integer
begin
    return 0;
end
