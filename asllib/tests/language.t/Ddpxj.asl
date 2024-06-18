//D_DPXJ: The syntax array [ expr ] of ty declares a single dimensional
//array of ty with an index type derived from the expression expr.

// RUN: interp %s | FileCheck %s

type a of array[10] of integer;

func main() => integer
begin
    return 0;
end
