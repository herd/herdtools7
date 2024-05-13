//R_DGJT: If expr is of type ty which has the structure of a
//well-constrained integer whose domain contains more than one value then
//the array is a constrained size array with number of indices equal to expr
//and constrained as per the constraint on expr.

// RUN: interp %s | FileCheck %s

config size: integer{0..10} = 5;

type a of array[size] of integer;

func main() => integer
begin
    return 0;
end
