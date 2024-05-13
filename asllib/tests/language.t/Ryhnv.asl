//R_YHNV: [The expression expr referred to in D_DPXJ below] must be either:
//- a non-negative, statically evaluable constrained integer expression
//expr, in which case the array has that many indices starting from 0
//- the name of a type which has the structure of an enumeration type, in
//which case the array’s indices are the enumeration literals of that type

//D_DPXJ: The syntax array [ expr ] of ty declares a single dimensional array
//of ty with an index type derived from the expression expr.

// RUN: interp %s | FileCheck %s

type a of enumeration {A, B};
type b of array [a] of integer;

func main() => integer
begin
    return 0;
end
