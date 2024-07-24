//R_PXRR: If expr is of type ty which has the structure of a
//well-constrained integer whose domain contains only one value then the
//array is a fixed size array with number of indices equal to the value in
//the domain of ty.

// RUN: interp %s | FileCheck %s

type a of array[10] of integer;

func main() => integer
begin
    return 0;
end
