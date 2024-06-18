//R_QZJS: If expr is of type ty which has the structure of a
//well-constrained integer whose domain contains only one value then
//bits(expr) denotes a fixed width bitvector whose determined width is
//equal to the value in the domain of ty.

// RUN: interp %s | FileCheck %s

config a: integer{10} = 10;

type b of bits(a);

func main() => integer
begin
    return 0;
end
